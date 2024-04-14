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

vTuple :: Parsec Text () Value
vTuple = do
  rs <- bracket "(" ")" $ commaSeparated $ value
  case rs of
    xs@(_ : _ : _) -> pure $ T xs
    _              -> fail "Not a tuple"

vString :: Parsec Text () Text
vString = T.pack <$> (single <|> double)
 where
  single = bracket "'" "'" $ inputs "'"
  double = bracket "\"" "\"" $ inputs "\""
  lchar :: [Char] -> Parsec Text () Char
  lchar extra = charExcept $ "\r\n" <> extra
  octal = do
    isOctal <- optionMaybe $ char '8'
    let
      base = case isOctal of
        Just _ -> 8
        Nothing -> 10
    a <- digit
    b <- digit
    c <- digit
    return $ chr $ (digitToInt a * base * base) + (digitToInt b * base) + digitToInt c
  qchar :: Parsec Text () Char
  qchar = do
    _ <- char '\\'
    octal <|> anyChar
  inputs :: [Char] -> Parsec Text () String
  inputs extra = many $ qchar <|> lchar extra

value :: Parsec Text () Value
value = choice
  [vTyped, vRecord, vList, vJson, vBool, vInt, vDouble, vUint32, vInt64, fmap S vString, vTuple, vVariant]

vVariant :: Parsec Text () Value
vVariant = fmap V $ bracket "<(" ")>" $ commaSeparated $ value

vList :: Parsec Text () Value
vList = fmap L $ bracket "[" "]" $ commaSeparated $ value

vTyped :: Parsec Text () Value
vTyped = do
  _ <- char '@'
  _ <- anyChar
  _ <- anyChar
  _ <- spaces
  value

vJson :: Parsec Text () Value
vJson = try $ bracket "'" "'" $ do
  _ <- lookAhead $ string "{"
  js <- many (charExcept "\r\n\'")
  pure $ Json (T.pack js)

vRecord :: Parsec Text () Value
vRecord = fmap R $ bracket "{" "}" $ commaSeparated $ do
  k <- vString
  _ <- char ':'
  _ <- spaces
  v <- value
  return (k,v)

dconfHeader :: Parsec Text () Header
dconfHeader = bracket "[" "]" $ do
  _ <- spaces
  h <- many1 $ satisfy $ \c -> isAlphaNum c || elem c ("/.-:_" :: [Char])
  _ <- spaces
  return $ T.pack h

kvLine :: Parsec Text () (Key,Value)
kvLine = do
  k <- many1 $ satisfy $ \c -> isAlphaNum c || c == '-'
  _ <- char '='
  v <- value
  _ <- endOfLine
  return (Key $ T.pack k,v)

entryParser :: Parsec Text () Entry
entryParser = do
  h  <- dconfHeader <* endOfLine
  kv <- many1 $ kvLine
  optional endOfLine
  pure $ Entry h (Map.fromList kv)

dconfParser :: Verbosity -> Parsec Text () [Entry]
dconfParser Normal  = manyTill entryParser eof
dconfParser Verbose = parserTraced "dconf" $ dconfParser Normal
