module DConfParser where

import Data.Functor ( (<&>) )
import Text.Parsec

type Field = String
data Value = S String | B Bool | I Int | I32 Int | D Double | T Value Value | L [Value] deriving Show

vBool :: Parsec String () Value
vBool = B False <$ string "false" <|> B True <$ string "true"

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

vUint32 :: Parsec String () Value
vUint32 = do
  many1 (string "uint32 ") >> spaces
  n <- many1 digit
  pure . I32 $ read n

vTuple :: Parsec String () Value
vTuple = try $ do
  parserTraced "tuple" $ char '('
  rs <- manyTill (vString `sepBy` (string "," >> spaces)) (char ')')
  case concat rs of
    (x:y:_) -> pure $ T x y
    _       -> parserFail "Not a tuple"

vString :: Parsec String () Value
vString = try $ do
  many1 (string "'")
  S . concat <$> manyTill inputs (string "'")
 where
  tokens    = many1 <$> [alphaNum, char '-', space, char ',', char '@', char '[', char ']', char '#']
  files     = many1 <$> [char ':', char '/', char '.']
  shortcuts = many1 <$> [char '<', char '>']
  inputs    = choice (tokens ++ files ++ shortcuts)

nixHeader :: Parsec String () Value
nixHeader = between (char '[') (char ']') vString

-- TODO: Rename
nixValues :: Parsec String () Value
nixValues = vBool <|> vInt <|> vDouble <|> vUint32 <|> vString <|> vTuple

vList :: Parsec String () Value
vList = try $ do
  char '['
  L . concat <$> manyTill (nixValues `sepBy` (string "," >> spaces)) (char ']')

nix = vList <|> nixValues

myParser :: IO ()
myParser = print . show $ runParser nix () "Err msg" "[true 3 false 2.2 asd]"
