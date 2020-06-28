module DConf where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intersperse)
import System.IO (appendFile, readFile, writeFile)

import DConfParser
import Text.Parsec

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
valueToNix raw = valueToNix' raw <> ";"
 where
  valueToNix' (S v)   = "\"" <> trim (takeWhile (/= '\'') v) <> "\""
  valueToNix' (B v)   = (toLower <$> show v)
  valueToNix' (I v)   = show v
  valueToNix' (I32 v) = "\"uint32 " <> show v <> "\""
  valueToNix' (D v)   = show v
  valueToNix' (T x y) = "mkTuple [ " <> valueToNix' x <> " " <> valueToNix' y <> " ]"
  valueToNix' (L xs)  =
    let ls = concat ((<> " ") <$> (valueToNix' <$> xs))
    in  "[ " <> ls <> "]"

---------- DConf to Entry -----------

parseContent :: [String] -> Content
parseContent [] = Map.fromList []
parseContent xs = Map.fromList (xs >>= f)
 where
  f [] = []
  f ys =
    let field = trim $ takeWhile (/= '=') ys
        raw   = trim $ drop (length field + 1) ys
        value = case runParser nix () raw raw of
          Left e  -> error (show e)
          Right v -> v
    in  [(field, value)]

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
  ls <- lines <$> readFile "./data/dconf.settings2"
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
