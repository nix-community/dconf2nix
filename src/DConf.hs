{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures, RankNTypes #-}

{- Parser combinators for dconf files (Gnome Shell) -}
module DConf
  ( dconfParser
  )
where

import           Codec.Binary.UTF8.String       ( encodeChar )
import           Control.Monad                  ( replicateM )
import           Data.Ix                        ( inRange )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Word                      ( Word8 )
import           DConf.Data
import           Text.Parsec
import           Data.Char

charExcept :: [Char] -> Parsec Text () Char
charExcept cc = satisfy $ \c -> not $ elem c cc

bracket :: String -> String -> Parsec Text () a -> Parsec Text () a
bracket s1 s2 pa = do
  -- Avoid consuming prefix to prevent conflict between b' and boolean cast.
  _ <- string' s1
  a <- pa
  _ <- string s2
  return a

comma :: Parsec Text () ()
comma = string "," >> spaces

commaSeparated :: Parsec Text () a -> Parsec Text () [a]
commaSeparated pa = sepBy pa comma

-- Like 'sepEndBy' but the separator can only appear at the end when there is just a single item.
sepOneEndBy :: Show a => Parsec Text () a -> Parsec Text () sep -> Parsec Text () [a]
sepOneEndBy p sep =
  do
    { x <- p
    ; _ <- sep
    ; xs <- sepBy p sep
    ; return (x:xs)
    }
    <|> return []

vBool :: Parsec Text () Value
vBool = B False <$ string "false" <|> B True <$ string "true"

vNothing :: Parsec Text () Value
vNothing = No <$ string "nothing"


numSign :: Num a => Parsec Text () a
numSign =
  (pure (-1) <* string "-")
  <|> (pure 1 <* string "+")
  <|> pure 1

vDouble :: Parsec Text () Value
vDouble = try $ do
  s <- numSign
  n <- many1 digit
  d <- string "."
  e <- many1 (digit <|> oneOf "eEdD-")
  pure . D $ s * read (n <> d <> e)

vInt :: Parsec Text () Value
vInt = try $ do
  s <- numSign
  (base, digitParser) <- prefix
  n <- (many1 digitParser <* notFollowedBy (char '.'))
    -- There was only a lone zero digit, already consumed by the octal prefix.
    <|> (if base == 8 then pure [0] else fail "Not a number")
  pure . I $ s * digitsToNum base n
 where
  prefix =
    ((pure (16, fromHexDigit <$> hexDigit) <* (string' "0x" <|> string' "0X"))
      -- @notFollowedBy@ to prevent ambiguity with @vDouble@.
      <|> (pure (8, fromOctDigit <$> octDigit) <* (string' "0" <* notFollowedBy (char '.')))
      <|> (pure (10, fromDecDigit <$> digit)))


vCast :: Parsec Text () Value
vCast = do
  ty <- choice $ map (\ty -> string' (castName ty) *> pure ty) (enumFrom (toEnum 0))
  _ <- spaces
  v <- value
  return (C ty v)

vTuple :: Parsec Text () Value
vTuple = T <$> (bracket "(" ")" $ sepOneEndBy value comma)

-- | Escape sequences common to strings and bytestrings
commonEscapes :: Parsec Text () (Maybe Char)
commonEscapes =
  -- The usual control sequence escapes `\a`, `\b`, `\f`, `\n`, `\r`, `\t` and `\v` are supported.
  (char 'a' *> pure (Just '\a'))
  <|> (char 'b' *> pure (Just '\b'))
  <|> (char 'f' *> pure (Just '\f'))
  <|> (char 'n' *> pure (Just '\n'))
  <|> (char 'r' *> pure (Just '\r'))
  <|> (char 't' *> pure (Just '\t'))
  <|> (char 'v' *> pure (Just '\v'))
  -- Additionally, a `\` before a newline character causes the newline to be ignored.
  <|> (char '\n' *> pure Nothing)
  -- Finally, any other character following `\` is copied literally (for example, `\"` or `\\`)
  <|> (Just <$> anyChar)

vString :: Parsec Text () Text
vString = T.pack <$> (single <|> double)
 where
  single = bracket "'" "'" $ inputs "'"
  double = bracket "\"" "\"" $ inputs "\""

  lchar :: [Char] -> Parsec Text () Char
  lchar extra = charExcept $ "\r\n\\" <> extra

  qchar :: Parsec Text () (Maybe Char)
  qchar = do
    _ <- char '\\'
    (
      -- Unicode escapes of the form `\uxxxx` and `\Uxxxxxxxx` are supported, in hexadecimal.
      (char 'u' *> (Just <$> (chr <$> hexNumFix 4)))
      <|> (char 'U' *> (Just <$> (chr <$> hexNumFix 8)))
      <|> commonEscapes)

  inputs :: [Char] -> Parsec Text () String
  inputs extra = catMaybes <$> (many $ qchar <|> (Just <$> lchar extra))

fromDecDigit :: Char -> Int
fromDecDigit n | inRange ('0', '9') n = ord n - ord '0'
fromDecDigit n = error $ "Expected a decimal digit, '" ++ n : "' given"

fromHexDigit :: Char -> Int
fromHexDigit n | inRange ('A', 'F') n = ord n - ord 'A' + 0xA
fromHexDigit n | inRange ('a', 'f') n = ord n - ord 'a' + 0xA
fromHexDigit n | inRange ('0', '9') n = ord n - ord '0'
fromHexDigit n = error $ "Expected a hexadecimal digit, '" ++ n : "' given"

fromOctDigit :: Char -> Int
fromOctDigit n | inRange ('0', '7') n = ord n - ord '0'
fromOctDigit n = error $ "Expected an octal digit, '" ++ n : "' given"

-- | Parses a hexadecimal number of precisely @l@ digits.
hexNumFix :: Int -> Parsec Text () Int
hexNumFix l = do
  digits <- replicateM l (fromHexDigit <$> hexDigit)
  return $ digitsToNum 16 digits

digitsToNum :: Int -> [Int] -> Int
digitsToNum base = foldl (\acc d -> base * acc + d) 0

-- | Parses an octal number between 1 and @maxlen@ digits.
octNumMax :: Int -> Parsec Text () Int
octNumMax maxLen = octNum' 0 maxLen
 where
  octNum' :: Int -> Int -> Parsec Text () Int
  octNum' acc 0 = return acc
  octNum' acc l = do
    d <- fromOctDigit <$> octDigit
    let acc' = acc * 8 + d
    octNum' acc' (l - 1) <|> octNum' acc' 0

vByteString :: Parsec Text () Value
vByteString = Bs <$> (single <|> double)
 where
  single = bracket "b'" "'" $ bs "'"
  double = bracket "b\"" "\"" $ bs "\""

  lchar :: [Char] -> Parsec Text () [Word8]
  lchar extra = encodeChar <$> (charExcept $ "\r\n\\" <> extra)

  qchar :: Parsec Text () [Word8]
  qchar = do
    _ <- char '\\'
    (
      -- Octal number (wrapped to 0-255)
      ((:[]) . fromIntegral . toInteger <$> octNumMax 3)
      <|> (maybe [] encodeChar <$> commonEscapes))

  bs :: [Char] -> Parsec Text () [Word8]
  bs extra = concat <$> (many $ qchar <|> lchar extra)

baseValue :: Parsec Text () Value
baseValue = choice
  [vBool, vInt, vDouble, fmap S vString]

value :: Parsec Text () Value
value = choice
  [vTyped, vDictDictEntry, vList, vJson, baseValue, vByteString, vCast, vNothing, vTuple, vVariant]

vVariant :: Parsec Text () Value
vVariant = fmap V $ bracket "<" ">" value

vList :: Parsec Text () Value
vList = fmap L $ bracket "[" "]" $ commaSeparated $ value

-- https://gitlab.gnome.org/GNOME/glib/-/blob/4607dd77a1944c6b29dff1e76c195f28a8ca12af/docs/reference/glib/gvariant-specification-1.0.rst#type-strings
typeString :: Parsec Text () String
typeString = baseType <|> containerType
  where
    baseType = choice (map string ["b", "y", "n", "q", "i", "u", "x", "t", "h", "d", "s", "o", "g"])
    containerType = variantType <|> maybeType <|> arrayType <|> tupleType <|> dictEntryType
    types = many typeString
    variantType = string "v"
    arrayType = do
      a <- string "a"
      t <- typeString
      return (a ++ t)
    maybeType = do
      m <- string "m"
      t <- typeString
      return (m ++ t)
    tupleType = do
      tt <- bracket "(" ")" types
      return ("(" ++ concat tt ++ ")")
    dictEntryType = bracket "{" "}" $ do
      k <- baseType
      v <- typeString
      return ("{" ++ k ++ v ++ "}")

vTyped :: Parsec Text () Value
vTyped = do
  _ <- char '@'
  t <- typeString
  _ <- spaces
  v <- value
  return (Ty t v)

vJson :: Parsec Text () Value
vJson = try $ bracket "'" "'" $ do
  _ <- lookAhead $ string "{"
  js <- many (charExcept "\r\n\'")
  pure $ Json (T.pack js)

-- | Parses a dictionary (@{k1: v1, k2: v2}@) or single dictionary entry (@{k, v}@)
vDictDictEntry :: Parsec Text () Value
vDictDictEntry = bracket "{" "}" $ do
  (do
    { 
    ; k <- baseValue
    ; isEntry <- (char ':' *> pure False) <|> (char ',' *> pure True)
    ; _ <- spaces
    ; v <- value
    ; if isEntry then
        return (DE k v)
      else do
        kvs <- (comma *> commaSeparated kvPair) <|> return []
        return (R ((k,v):kvs))
    }
    <|> return (R []))

  where
    kvPair = do
      k <- baseValue
      _ <- char ':'
      _ <- spaces
      v <- value
      return (k,v)

dconfHeader :: Parsec Text () Header
dconfHeader = bracket "[" "]" $ do
  h <- many1 $ satisfy $ \c -> not (isControl c) && not (elem c ("[]" :: [Char]))
  return $ T.pack h

kvLine :: Parsec Text () (Key,Value)
kvLine = do
  k <- many1 $ satisfy $ \c -> isAlphaNum c || c == '-' || c == '_'
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
