{-# LANGUAGE LambdaCase #-}

module DConf (
  parseEntry
  --parseEntry'
) where

import Data.Functor ( (<&>) )
import qualified Data.Map as Map
import Domain
import Text.Parsec

vBool :: Parsec String () Value
vBool = B False <$ string "false" <|> B True <$ string "true"

vDouble :: Parsec String () Value
vDouble = do
  s <- option "" $ string "-"
  n <- many1 digit
  d <- string "."
  e <- many1 digit
  pure . D $ read (s <> n <> d <> e)

vInt :: Parsec String () Value
vInt = try $ do
  s <- option "" $ string "-"
  n <- many1 digit <* notFollowedBy (char '.')
  pure . I $ read (s <> n)

vUint32 :: Parsec String () Value
vUint32 = do
  many1 (string "uint32 ") >> spaces
  n <- many1 digit
  pure . I32 $ read n

vInt64 :: Parsec String () Value
vInt64 = do
  many1 (string "int64 ") >> spaces
  n <- many1 digit
  pure . I64 $ read n

vTuple :: Parsec String () Value
vTuple = try $ do
  char '('
  rs <- manyTill (dconf `sepBy` (string "," >> spaces)) (char ')')
  case concat rs of
    (x:y:_) -> pure $ T x y
    _       -> parserFail "Not a tuple"

vTupleInList :: Parsec String () Value
vTupleInList = vTuple <&> \case
  T x y -> TL x y
  a     -> a

vEmptyString :: Parsec String () Value
vEmptyString = S "" <$ try (string "''")

vString :: Parsec String () Value
vString = try $ do
  many1 (string "'")
  S . concat <$> manyTill inputs (string "'")
 where
  tokens    = many1 <$> [alphaNum, space] ++ (char <$> "-_()[],#@")
  files     = many1 <$> char <$> ":/."
  shortcuts = many1 <$> char <$> "<>"
  inputs    = choice (tokens ++ files ++ shortcuts)

vAny :: Parsec String () Value
vAny = S <$> try (manyTill anyChar eof)

dconf :: Parsec String () Value
dconf = choice [vBool, vInt, vDouble, vUint32, vInt64, vEmptyString, vString, vTuple, vAny]

vList :: Parsec String () Value
vList = try $ do
  char '['
  L . concat <$> manyTill ((vTupleInList <|> dconf) `sepBy` (string "," >> spaces)) (char ']')

dconfHeader :: Parsec String () String
dconfHeader = do
  many1 (char '[') <* spaces
  concat <$> manyTill tokens (string " ]" <|> string "]")
 where
  tokens = choice $ many1 <$> [char '/', char '-', alphaNum]

dconfValue :: Parsec String () Value
dconfValue = vList <|> dconf
--dconfValue = parserTraced "dconf" $ vList <|> dconf

---------------------------------------------------------

vKey :: Parsec String () Key
vKey = manyTill (choice [alphaNum, char '-']) (char '=')

vContent :: Parsec String () (Key, Value)
vContent = do
  k <- vKey
  v <- dconfValue
  pure (k,v)

parseContent :: [String] -> Content
parseContent [] = Map.fromList []
parseContent xs = Map.fromList $
  xs >>= \x -> case runParser vContent () x x of
    Left _  -> []
    Right x -> pure x

parseEntry :: [String] -> Maybe Entry
parseEntry []      = Nothing
parseEntry (h : t) = Just (Entry (parseHeader h) (parseContent t))

--parseEntry' :: Parsec [String] () Entry
--parseEntry' = do
  --getInput >>= \case
    --[] -> fail "empty input"
    --(x:xs) -> do
      --h <- many dconfHeader
      --traverse vContent xs

parseHeader :: String -> String
parseHeader s = case runParser dconfHeader () s s of
  Left e  -> error (show e)
  Right v -> v
