module Main where

import           Control.Monad                  ( unless )
import           DConfTest                      ( dconfParserTests )
import           DConf2NixTest                  ( dconf2nixHmTests, dconf2nixNixOSTests )
import           Hedgehog
import           System.Exit

main :: IO ()
main = do
  results <- sequence
    [ checkParallel dconfParserTests
    , checkParallel dconf2nixHmTests
    , checkParallel dconf2nixNixOSTests]
  unless (and results) exitFailure
