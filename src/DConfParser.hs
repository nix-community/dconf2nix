{-# LANGUAGE LambdaCase #-}

module DConfParser where

import Data.Functor ( (<&>) )
import DConf
import Text.Parsec
import Text.Parsec.Number

vBool :: Parsec String () Value
vBool = (string "false" <|> string "true") <&> \case
  "false" -> B False
  "true"  -> B True

vDouble :: Parsec String () Value
vDouble = D <$> floating

vInt :: Parsec String () Value
vInt = I <$> nat

vString :: Parsec String () Value
vString = S <$> many alphaNum

--parseList :: String -> Maybe Value
--parseList ('[':xs) = Just (L $ parseValue <$> words (takeWhile (/= ']') xs))
--parseList _        = Nothing

vList :: Parsec String () Value
vList = undefined

nixValues = vBool <|> vInt <|> vDouble <|> vString

nix = nixValues

myParser :: IO ()
myParser = print . show $ runParser nix () "Err msg" "asd16#"
