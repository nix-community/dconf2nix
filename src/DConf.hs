module DConf where

import System.IO (readFile)

data Entry = Entry
  { header :: String
  , content :: [String]
  } deriving Show

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
  ls <- lines <$> readFile "./data/dconf.settings"
  iter ls
 where
  iter [] = pure ()
  iter xs = do
    let e = takeWhile (/= []) xs
    print . show $ parseEntry e
    iter $ drop (length e + 1) xs
