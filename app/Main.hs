{-# LANGUAGE LambdaCase #-}

module Main where

import           CommandLine                    ( Args(..)
                                                , FileArgs(..)
                                                , Input(..)
                                                , runArgs
                                                )
import           DConf2Nix                      ( dconf2nixFile
                                                , dconf2nixStdin
                                                )

main :: IO ()
main = runArgs >>= \case
  Args r s v (FileInput (FileArgs i o)) ->
    dconf2nixFile i o r s v
  Args r s v StdinInput   ->
    dconf2nixStdin r s v
