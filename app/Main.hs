{-# LANGUAGE LambdaCase #-}

module Main where

import Domain               ( unNix         )
import DConf                ( dconfParser   )
import qualified Nix
import Text.Parsec          ( runParser     )
import Text.Parsec.String   ( parseFromFile )

-- TODO: Parse args for input and output file
main :: IO ()
main = do
  writeFile output Nix.renderHeader
  parseFromFile dconfParser "./data/dconf.settings2" >>= \case
    Left err -> error (show err)
    Right xs -> traverse (\e -> appendFile output (unNix $ Nix.renderEntry e)) xs
  appendFile output "}"
 where
  output  = "./output/dconf2.nix"
