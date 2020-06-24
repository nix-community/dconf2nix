module DConf where

import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (appendFile, readFile, writeFile)

type Field = String
data Value = S String | B Bool | I Int | D Double | L [Value] deriving Show

type Header  = String
type Content = Map Field Value

data Entry = Entry
  { header :: Header
  , content :: Content
  } deriving Show

type Nix = String

entryToNix :: Entry -> Nix
entryToNix (Entry h c) =
  let header = "  \"" ++ h ++ "\" = {\n"
      body   = (Map.toList c) >>= (\(f, (S v)) -> "    \"" ++ f ++ "\"=" ++ v ++ "\n")
      close  = "  }\n"
  in header ++ body ++ close

parseContent :: [String] -> Content
parseContent []   = Map.fromList []
parseContent xs = Map.fromList (xs >>= f)
 where
  f [] = []
  f ys =
    let field = trim $ takeWhile (/= '=') ys
        value = S . trim $ drop (length field + 1) ys
    in  [(field, value)]

parseEntry :: [String] -> Maybe Entry
parseEntry []      = Nothing
parseEntry (h : t) = Just (Entry (parseHeader h) (parseContent t))

trim :: String -> String
trim xs =
  let trim' ys = dropWhile (== ' ') ys
  in  trim' (reverse $ trim' (reverse xs))

parseHeader :: String -> String
parseHeader [] = []
parseHeader ('[' : t) = parseHeader (reverse t)
parseHeader (']' : t) = trim (reverse t)

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
