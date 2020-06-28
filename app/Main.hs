{-# LANGUAGE LambdaCase #-}

module Main where

import           CommandLine                    ( Args(..)
                                                , runArgs
                                                )
import           DConf2Nix                      ( dconf2nix )

main :: IO ()
main = runArgs >>= \case
  (Args i o) -> dconf2nix i o
