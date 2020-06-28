{-# LANGUAGE LambdaCase, OverloadedStrings #-}

{- Parser combinators for dconf files (Gnome Shell) -}
module DConf
  ( dconfParser
  )
where

import           Data.Functor                   ( (<&>) )
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Domain
import           Text.Parsec

vBool :: Parsec Text () Value
vBool = B False <$ string "false" <|> B True <$ string "true"

vDouble :: Parsec Text () Value
vDouble = do
  s <- option "" $ string "-"
  n <- many1 digit
  d <- string "."
  e <- many1 digit
  pure . D $ read (s <> n <> d <> e)

vInt :: Parsec Text () Value
vInt = try $ do
  s <- option "" $ string "-"
  n <- many1 digit <* notFollowedBy (char '.')
  pure . I $ read (s <> n)

vUint32 :: Parsec Text () Value
vUint32 = do
  many1 (string "uint32 ") >> spaces
  n <- many1 digit
  pure . I32 $ read n

vInt64 :: Parsec Text () Value
vInt64 = do
  many1 (string "int64 ") >> spaces
  n <- many1 digit
  pure . I64 $ read n

vTuple :: Parsec Text () Value
vTuple = try $ do
  char '('
  rs <- manyTill (dconf `sepBy` (string "," >> spaces)) (char ')')
  case concat rs of
    (x : y : _) -> pure $ T x y
    _           -> fail "Not a tuple"

vTupleInList :: Parsec Text () Value
vTupleInList = vTuple <&> \case
  T x y -> TL x y
  a     -> a

vEmptyString :: Parsec Text () Value
vEmptyString = S "" <$ try (string "''")

vString :: Parsec Text () Value
vString = try $ do
  many1 (string "'")
  S . T.pack . concat <$> manyTill inputs (string "'")
 where
  tokens    = many1 <$> [alphaNum, space] ++ (char <$> "-_()[],#@")
  files     = many1 . char <$> ":/."
  shortcuts = many1 . char <$> "<>"
  inputs    = choice (tokens ++ files ++ shortcuts)

vAny :: Parsec Text () Value
vAny = S . T.pack <$> manyTill anyChar endOfLine

dconf :: Parsec Text () Value
dconf = choice
  [vBool, vInt, vDouble, vUint32, vInt64, vEmptyString, vString, vTuple, vAny]

vList :: Parsec Text () Value
vList = try $ do
  char '['
  L . concat <$> manyTill
    ((vTupleInList <|> dconf) `sepBy` (string "," >> spaces))
    (char ']')

dconfHeader :: Parsec Text () Header
dconfHeader = do
  many1 (char '[') <* spaces
  T.pack . concat <$> manyTill tokens (string " ]" <|> string "]")
  where tokens = choice $ many1 <$> [char '/', char '-', alphaNum]

dconfValue :: Parsec Text () Value
dconfValue = vList <|> dconf

vKey :: Parsec Text () Key
vKey = Key . T.pack <$> manyTill (choice [alphaNum, char '-']) (char '=')

-- To debug insert `parserTraced "name" $` before the parser
entryParser :: Parsec Text () Entry
entryParser = do
  h  <- dconfHeader <* endOfLine
  kv <- many1 ((,) <$> vKey <*> (dconfValue <* endOfLine))
  optional endOfLine
  pure $ Entry h (Map.fromList kv)

dconfParser :: Parsec Text () [Entry]
dconfParser = manyTill entryParser eof
