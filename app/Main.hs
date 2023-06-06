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
  FileInput (FileArgs i o r e v) ->
    dconf2nixFile i o r e v
  StdinInput (StdinArgs r e v)   ->
    dconf2nixStdin r e v
