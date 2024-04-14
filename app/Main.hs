{-# LANGUAGE LambdaCase #-}

module Main where

import           CommandLine                    ( FileArgs(..)
                                                , Input(..)
                                                , StdinArgs(..)
                                                , runArgs
                                                )
import           DConf2Nix                      ( dconf2nixFile
                                                , dconf2nixStdin
                                                )

main :: IO ()
main = runArgs >>= \case
  FileInput (FileArgs i o r v) ->
    dconf2nixFile i o r v
  StdinInput (StdinArgs r v)   ->
    dconf2nixStdin r v
