{-# LANGUAGE LambdaCase #-}

module Main where

import Domain               ( unNix         )
import DConf                ( dconfParser   )
import qualified Nix
import System.Environment   ( getArgs       )
import Text.Parsec          ( runParser     )
import Text.Parsec.String   ( parseFromFile )

-- TODO: Use optparse-applicative before releasing it as a binary?
main :: IO ()
main = getArgs >>= \case
  [i, o] -> do
    putStrLn $ "Input: " <> i
    putStrLn $ "Output: " <> o
    dconf2nix i o
  _      -> do
    putStrLn "Missing `input` and `output` arguments, using default values."
    dconf2nix "./data/dconf.settings" "./output/dconf.nix"

dconf2nix :: FilePath -> FilePath -> IO ()
dconf2nix input output = do
  writeFile output Nix.renderHeader
  parseFromFile dconfParser input >>= \case
    Left err -> error (show err)
    Right xs -> traverse (\e -> appendFile output (unNix $ Nix.renderEntry e)) xs
  appendFile output "}"
