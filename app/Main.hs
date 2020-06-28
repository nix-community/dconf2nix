module Main where

import Domain (unNix)
import qualified DConf
import qualified Nix
--import Text.Parsec (runParser)

-- TODO: Parse args for input and output file
-- TODO: Use `parseFromFile` maybe?
main :: IO ()
main = do
  ls <- lines <$> readFile "./data/dconf.settings"
  writeFile output Nix.renderHeader
  appendFile output (iter ls)
  appendFile output "}"
 where
  output  = "./output/dconf.nix"
  iter [] = ""
  iter xs =
    let e  = takeWhile (/= []) xs
        ys = case DConf.parseEntry e of
          Nothing -> ""
          Just v  -> unNix $ Nix.renderEntry v
        --ys = case runParser DConf.parseEntry' () (show e) e of
          --Left e  -> show e
          --Right v -> Nix.renderEntry v
    in  ys <> (iter $ drop (length e + 1) xs)
