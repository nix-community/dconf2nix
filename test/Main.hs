module Main where

import           Control.Monad                  ( unless )
import           DConfTest
import           Hedgehog
import           System.Exit

main :: IO ()
main = do
  results <- sequence [checkParallel dconfParserTests]
  unless (and results) exitFailure
