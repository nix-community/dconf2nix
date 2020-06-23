module DConf where

import Data.Map (Map)
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)

import qualified Text.Parsec.Expr as Expr
import qualified Text.Parsec.Token as Token
import System.IO (readFile)

type Field = String

data Value = S String | B Bool | I Int | D Double | L [Value] deriving Show

type Header  = String
type Content = Map Field Value

data Entry = Entry
  { header :: Header
  , content :: [String]
  } deriving Show

parseContent :: [String] -> Content
parseContent = undefined

parseEntry :: [String] -> Maybe Entry
parseEntry []      = Nothing
parseEntry (h : t) = Just (Entry (parseHeader h) t)

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
  ls <- lines <$> readFile "./data/dconf.easy"
  iter ls
 where
  iter [] = pure ()
  iter xs = do
    let e = takeWhile (/= []) xs
    print . show $ parseEntry e
    iter $ drop (length e + 1) xs

--------------------------------------------

langDef :: Token.LanguageDef ()
langDef = Token.LanguageDef
  { Token.commentStart    = "{-"
  , Token.commentEnd      = "-}"
  , Token.commentLine     = "--"
  , Token.nestedComments  = True
  , Token.identStart      = letter
  , Token.identLetter     = alphaNum <|> oneOf "_'"
  , Token.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Token.reservedNames   = []
  , Token.reservedOpNames = []
  , Token.caseSensitive   = True
  }
