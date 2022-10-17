{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures, RankNTypes #-}

{- Parser combinators for dconf files (Gnome Shell) -}
module DConf
  ( dconfParser
  )
where

import           Data.Functor                   ( (<&>) )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           DConf.Data
import           Text.Emoji                     ( emojis )
import           Text.Parsec

type Parser
  =  forall s (m :: * -> *) t u a end
   . Stream s m t
  => ParsecT s u m a
  -> ParsecT s u m end
  -> ParsecT s u m [a]

vBool :: Parsec Text () Value
vBool = B False <$ string "false" <|> B True <$ string "true"

vDouble :: Parsec Text () Value
vDouble = try $ do
  s <- option "" $ string "-"
  n <- many1 digit
  d <- string "."
  e <- many1 (digit <|> oneOf "eEdD-")
  pure . D $ read (s <> n <> d <> e)

vInt :: Parsec Text () Value
vInt = try $ do
  s <- option "" $ string "-"
  n <- many1 digit <* notFollowedBy (char '.')
  pure . I $ read (s <> n)

vUint32 :: Parsec Text () Value
vUint32 = try $ do
  many1 (string "uint32 ") >> spaces
  I32 . read <$> many1 digit

vInt64 :: Parsec Text () Value
vInt64 = try $ do
  many1 (string "int64 ") >> spaces
  I64 . read <$> many1 digit

vTuple :: EmojiSupport -> Parsec Text () Value
vTuple es = try $ do
  char '('
  rs <- manyTill (dconf es manyTill `sepBy` (string "," >> spaces)) (char ')')
  case concat rs of
    xs@(_ : _ : _) -> pure $ T xs
    _              -> fail "Not a tuple"

vEmptyString :: Parsec Text () Value
vEmptyString = S "" <$ (try (string "''") <|> try (string "\"\""))

vEmoji :: Parsec Text () Value
vEmoji =
  let e = T.head . snd <$> emojis
      f = choice $ char <$> e
      s = many1 (string "'") *> f <* string "'"
      d = many1 (char '"') *> f <* char '"'
  in  Emo <$> try (s <|> d)

vString :: Parser -> Parsec Text () Value
vString parser = try $ do
  S . T.pack . concat <$> (single <|> double)
 where
  single = many1 (string "'") *> parser inputs (string "'")
  double = many1 (char '"') *> parser (inputs <|> string "'") (char '"')
  tokens = many1 <$> [alphaNum, space] ++ (char <$> "=$!&+-_()[]{}|,#@\\")
  files  = many1 . char <$> ":/."
  shorts = many1 . char <$> "<>"
  inputs = choice (tokens ++ files ++ shorts)

vAny :: Parsec Text () Value
vAny = S . T.pack <$> manyTill anyChar (try $ lookAhead endOfLine)

emojiParser :: EmojiSupport -> [Parsec Text () Value]
emojiParser Enabled  = [vEmoji]
emojiParser Disabled = []

dconf :: EmojiSupport -> Parser -> Parsec Text () Value
dconf es p =
  let xs = [vBool, vInt, vDouble, vUint32, vInt64, vEmptyString]
      ys = [vString p, vTuple es, vAny]
  in  choice (xs ++ emojiParser es ++ ys)

-- There is no support for variants in HM yet so we parse them as a string
vListOfVariant :: Parsec Text () Value
vListOfVariant =
  let variant1 = try $ do
        try (lookAhead $ string "[<") <|> try (lookAhead $ string "[{")
        manyTill anyToken (try $ lookAhead endOfLine)
      variant2 = try $ do
        try (lookAhead $ string "\"[<") <|> try (lookAhead $ string "\"[{")
        char '"'
        manyTill anyToken (try $ lookAhead $ string "\"") <* char '"'
  in  S . T.pack <$> (variant1 <|> variant2)

vList :: EmojiSupport -> Parsec Text () Value
vList es = try $ do
  char '['
  L . concat <$> manyTill
    ((vJson <|> dconf es manyTill) `sepBy` (string "," >> spaces))
    (char ']')

vJson :: Parsec Text () Value
vJson = try $ do
  try (lookAhead $ string "'{")
  char '\''
  js <- manyTill anyToken (try $ lookAhead $ char '\'')
  Json (T.pack js) <$ char '\''

vEmptyList :: Parsec Text () Value
vEmptyList = EmptyList <$ try (string "@as []")

dconfHeader :: Parsec Text () Header
dconfHeader = do
  many1 (char '[') <* spaces
  T.pack . concat <$> manyTill tokens (string " ]" <|> string "]")
  where tokens = choice $ many1 <$> [oneOf "/.-:_",  alphaNum]

dconfValue :: EmojiSupport -> Parsec Text () Value
dconfValue es = vListOfVariant <|> vList es <|> vEmptyList <|> vJson <|> dconf es endBy

vKey :: Parsec Text () Key
vKey = Key . T.pack <$> manyTill (choice [alphaNum, char '-']) (char '=')

entryParser :: EmojiSupport -> Parsec Text () Entry
entryParser es = do
  h  <- dconfHeader <* endOfLine
  kv <- many1 ((,) <$> vKey <*> (dconfValue es <* endOfLine))
  optional endOfLine
  pure $ Entry h (Map.fromList kv)

dconfParser :: EmojiSupport -> Verbosity -> Parsec Text () [Entry]
dconfParser es Normal  = manyTill (entryParser es) eof
dconfParser es Verbose = parserTraced "dconf" $ dconfParser es Normal
