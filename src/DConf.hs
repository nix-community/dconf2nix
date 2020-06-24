{-# LANGUAGE ScopedTypeVariables, RankNTypes #-}

module DConf where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
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
      body   = (Map.toList c) >>= (\(f, v) -> "    \"" ++ f ++ "\"=" ++ parseValue v ++ "\n")
      close  = "  };\n\n"
  in header ++ body ++ close

parseValue :: Value -> String
parseValue (S v) = "\"" ++ trim (takeWhile (/= '\'') (drop 1 v)) ++ "\";"
parseValue (B v) = (toLower <$> show v) ++ ";"
parseValue (D v) = show v ++ ";"
parseValue _     = ""

---------- DConf to Entry -----------

parseContent :: [String] -> Content
parseContent []   = Map.fromList []
parseContent xs = Map.fromList (xs >>= f)
 where
  f [] = []
  f ys =
    let field = trim $ takeWhile (/= '=') ys
        raw   = trim $ drop (length field + 1) ys
        value = case raw of
          "false" -> B False
          "true"  -> B True
          v       -> case (readNumber v :: Maybe Double) of
            Nothing -> S v
            Just n  -> D n
    in  [(field, value)]

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
