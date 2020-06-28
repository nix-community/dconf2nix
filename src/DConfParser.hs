module DConfParser where

import Data.Functor ( (<&>) )
import Text.Parsec

type Field = String
data Value = S String | B Bool | I Int | D Double | L [Value] deriving Show

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
  tokens    = many1 <$> [alphaNum, char '-', space, char ',', char '@', char '[', char ']', char '#']
  files     = many1 <$> [char ':', char '/', char '.']
  shortcuts = many1 <$> [char '<', char '>']
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
