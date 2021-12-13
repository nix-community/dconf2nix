{-# LANGUAGE LambdaCase #-}

module Main where

import           CommandLine                    ( FileArgs(..)
                                                , Input(..)
                                                , StdinArgs(..)
                                                , runArgs
                                                )
import           Data.Foldable                  ( traverse_ )
import           DConf.Data                     ( ProcessTimeout(..)
                                                , Verbosity(..)
                                                )
import           DConf2Nix                      ( dconf2nixFile
                                                , dconf2nixStdin
                                                )
import           System.Timeout                 ( timeout )

timeoutMessage = unlines
  [ "💥 The process timed out."
  , ""
  , "  💡 You can try increasing the timeout using --timeout."
  , ""
  , "  ⛔ If the issue persists, run it again using --verbose and report the issue on Github, indicating dconf2nix's version. Sorry 😞."
  ]

dconf2nix :: ProcessTimeout -> IO () -> Maybe String -> IO ()
dconf2nix (ProcessTimeout t) fa successMsg = timeout (t * 1000000) fa >>= \case
  Just _  -> traverse_ putStrLn successMsg
  Nothing -> error timeoutMessage

main :: IO ()
main = runArgs >>= \case
  FileInput (FileArgs i o r t v) ->
    dconf2nix t (dconf2nixFile i o r v) (Just "🚀 Successfully Nixified! ❄️")
  StdinInput (StdinArgs r t v)   ->
    dconf2nix t (dconf2nixStdin r v) Nothing
