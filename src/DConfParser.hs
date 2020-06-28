{-# LANGUAGE LambdaCase #-}

module DConfParser where

import Data.Functor ( (<&>) )
import Text.Parsec

type Field = String
data Value = S String | B Bool | I Int | D Double | L [Value] deriving Show

vBool :: Parsec String () Value
vBool = (string "false" <|> string "true") <&> \case
  "false" -> B False
  "true"  -> B True

vDouble :: Parsec String () Value
vDouble = do
  s <- try (many1 (char '-'))
  n <- many1 digit
  d <- string "."
  e <- many1 digit
  pure . D $ read (s <> n <> d <> e)

vInt :: Parsec String () Value
vInt = try $
  I . read <$> many1 digit <* notFollowedBy (char '.')

symbol :: Char -> Parsec String () String
symbol = many1 . char

-- TODO: Introduce proper type
vUint32 :: Parsec String () Value
vUint32 = do
  many1 (string "uint32 ") >> spaces
  S <$> ((++) "uint32 ") <$> many1 digit

vString :: Parsec String () Value
vString = try $ do
  many1 (string "'" >> spaces)
  S . concat <$> manyTill inputs (try (string "'"))
 where
  tokens    = [many1 alphaNum, symbol '-', many1 space, symbol ',',
               symbol '@', symbol '[', symbol ']', symbol '#']
  files     = [symbol ':', symbol '/', symbol '.']
  shortcuts = [symbol '<', symbol '>']
  inputs    = choice (tokens ++ files ++ shortcuts)

nixHeader :: Parsec String () Value
nixHeader = between (char '[') (char ']') vString

-- TODO: Rename
nixValues :: Parsec String () Value
nixValues = vBool <|> vInt <|> vDouble <|> vUint32 <|> vString

vList :: Parsec String () Value
vList = do
  string "["
  rs <- manyTill (nixValues `sepBy` string ", ") (try (string "]"))
  pure $ L (concat rs)

nix = nixValues <|> vList

myParser :: IO ()
myParser = print . show $ runParser nix () "Err msg" "[true 3 false 2.2 asd]"
