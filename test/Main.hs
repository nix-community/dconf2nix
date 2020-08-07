module Main where

import           Control.Monad                  ( unless )
import           DConfTest                      ( dconfParserTests )
import           DConf2NixTest                  ( dconf2nixTests )
import           Hedgehog
import           System.Exit

main :: IO ()
main = do
  results <- sequence
    [checkParallel dconfParserTests, checkParallel dconf2nixTests]
  unless (and results) exitFailure
