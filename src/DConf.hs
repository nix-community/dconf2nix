{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module DConf where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersperse)
import System.IO (appendFile, readFile, writeFile)

type Field = String
data Value = S String | B Bool | I Int | D Double | L [Value] deriving Show

type Nix = String

type Header  = String
type Content = Map Field Value

data Entry = Entry
  { header :: Header
  , content :: Content
  } deriving Show

---------- Entry to Nix -----------

entryToNix :: Entry -> Nix
entryToNix (Entry h c) =
  let header = "  \"" ++ h ++ "\" = {\n"
      body   = (Map.toList c) >>= (\(f, v) -> "    \"" ++ f ++ "\" = " ++ valueToNix v ++ "\n")
      close  = "  };\n\n"
  in header ++ body ++ close

valueToNix :: Value -> Nix
valueToNix raw = valueToNix' raw ++ ";"
 where
  valueToNix' (S v)  = "\"" ++ trim (takeWhile (/= '\'') (drop 1 v)) ++ "\""
  valueToNix' (B v)  = (toLower <$> show v)
  valueToNix' (I v)  = show v
  valueToNix' (D v)  = show v
  valueToNix' (L xs) =
    let ls = concat ((\c -> c ++ " ") <$> (valueToNix' <$> xs))
    in  "[ " ++ ls ++ "]"

---------- DConf to Entry -----------

-- TODO: use parser combinators
parseContent :: [String] -> Content
parseContent [] = Map.fromList []
parseContent xs = Map.fromList (xs >>= f)
 where
  f [] = []
  f ys =
    let field = trim $ takeWhile (/= '=') ys
        raw   = trim $ drop (length field + 1) ys
        value = case parseList raw of
          Nothing -> parseValue raw
          Just l  -> l
    in  [(field, value)]

parseValue :: String -> Value
parseValue raw = case raw of
  "false" -> B False
  "true"  -> B True
  v       -> numberOr v
 where
  numberOr :: String -> Value
  numberOr v = case (readNumber v :: Maybe Int) of
    Nothing -> case (readNumber v :: Maybe Double) of
      Nothing -> S v
      Just n  -> D n
    Just n  -> I n

parseList :: String -> Maybe Value
parseList ('[':xs) = Just (L $ parseValue <$> words (takeWhile (/= ']') xs))
parseList _        = Nothing

readNumber :: forall a . (Num a, Read a) => String -> Maybe a
readNumber s =
  case (reads s :: [(a, String)]) of
    (i, _):_ -> Just i
    []       -> Nothing

parseEntry :: [String] -> Maybe Entry
parseEntry []      = Nothing
parseEntry (h : t) = Just (Entry (parseHeader h) (parseContent t))

parseHeader :: String -> String
parseHeader [] = []
parseHeader ('[' : t) = parseHeader (reverse t)
parseHeader (']' : t) = trim (reverse t)

----------- Main function ------------

parseDconf :: IO ()
parseDconf = do
  ls <- lines <$> readFile "./data/dconf.settings"
  writeFile output "{\n" -- override if it exists
  appendFile output (iter ls)
  appendFile output "}"
 where
  output  = "./output/dconf.nix"
  iter [] = ""
  iter xs =
    let e  = takeWhile (/= []) xs
        ys = case parseEntry e of
          Nothing -> ""
          Just e  -> entryToNix e
    in  ys ++ (iter $ drop (length e + 1) xs)

------------ Utils -----------------

trim :: String -> String
trim xs =
  let trim' ys = dropWhile (== ' ') ys
  in  trim' (reverse $ trim' (reverse xs))
