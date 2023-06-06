{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures, RankNTypes #-}

{- Parser combinators for dconf files (Gnome Shell) -}
module DConf
  ( dconfParser
  )
where

import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           DConf.Data
import           Text.Emoji                     ( emojis )
import           Text.Parsec
import           Data.Char

charExcept :: [Char] -> Parsec Text () Char
charExcept cc = satisfy $ \c -> not $ elem c cc

bracket :: String -> String -> Parsec Text () a -> Parsec Text () a
bracket s1 s2 pa = do
  _ <- string s1
  a <- pa
  _ <- string s2
  return a

commaSeparated :: Parsec Text () a -> Parsec Text () [a]
commaSeparated pa = sepBy pa $ string "," >> spaces

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
vTuple es = do
  rs <- bracket "(" ")" $ commaSeparated $ value es
  case rs of
    xs@(_ : _ : _) -> pure $ T xs
    _              -> fail "Not a tuple"

vEmoji :: Parsec Text () Value
vEmoji =
  let e = T.head . snd <$> emojis
      f = choice $ char <$> e
      s = many1 (string "'") *> f <* string "'"
      d = many1 (char '"') *> f <* char '"'
  in  Emo <$> try (s <|> d)

vString :: Parsec Text () Text
vString = T.pack <$> (single <|> double)
 where
  single = bracket "'" "'" $ inputs "'"
  double = bracket "\"" "\"" $ inputs "\""
  lchar :: [Char] -> Parsec Text () Char
  lchar extra = charExcept $ "\r\n" <> extra
  qchar :: Parsec Text () Char
  qchar = char '\\' >> anyChar
  inputs :: [Char] -> Parsec Text () String
  inputs extra = many $ qchar <|> lchar extra

emojiParser :: EmojiSupport -> [Parsec Text () Value]
emojiParser Enabled  = [vEmoji]
emojiParser Disabled = []

value :: EmojiSupport -> Parsec Text () Value
value es =
  let xs = [vTyped es, vRecord es, vList es, vJson, vBool, vInt, vDouble, vUint32, vInt64]
      ys = [fmap S vString, vTuple es, vVariant es]
  in  choice (xs ++ emojiParser es ++ ys)

vVariant :: EmojiSupport -> Parsec Text () Value
vVariant es = fmap V $ bracket "<(" ")>" $ commaSeparated $ value es

vList :: EmojiSupport -> Parsec Text () Value
vList es = fmap L $ bracket "[" "]" $ commaSeparated $ value es

vTyped :: EmojiSupport -> Parsec Text () Value
vTyped es = do
  _ <- char '@'
  _ <- anyChar
  _ <- anyChar
  _ <- spaces
  value es

vJson :: Parsec Text () Value
vJson = try $ bracket "'" "'" $ do
  _ <- lookAhead $ string "{"
  js <- many (charExcept "\r\n\'")
  pure $ Json (T.pack js)

vRecord :: EmojiSupport -> Parsec Text () Value
vRecord es = fmap R $ bracket "{" "}" $ commaSeparated $ do
  k <- vString
  _ <- char ':'
  _ <- spaces
  v <- value es
  return (k,v)

dconfHeader :: Parsec Text () Header
dconfHeader = bracket "[" "]" $ do
  _ <- spaces
  h <- many1 $ satisfy $ \c -> isAlphaNum c || elem c ("/.-:_" :: [Char])
  _ <- spaces
  return $ T.pack h

kvLine :: EmojiSupport -> Parsec Text () (Key,Value)
kvLine es = do
  k <- many1 $ satisfy $ \c -> isAlphaNum c || c == '-'
  _ <- char '='
  v <- value es
  _ <- endOfLine
  return (Key $ T.pack k,v)

entryParser :: EmojiSupport -> Parsec Text () Entry
entryParser es = do
  h  <- dconfHeader <* endOfLine
  kv <- many1 $ kvLine es
  optional endOfLine
  pure $ Entry h (Map.fromList kv)

dconfParser :: EmojiSupport -> Verbosity -> Parsec Text () [Entry]
dconfParser es Normal  = manyTill (entryParser es) eof
dconfParser es Verbose = parserTraced "dconf" $ dconfParser es Normal
