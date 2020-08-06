{-# LANGUAGE LambdaCase #-}

module Main where

import           CommandLine                    ( Args(..)
                                                , runArgs
                                                )
import           DConf.Data                     ( ProcessTimeout(..) )
import           DConf2Nix                      ( dconf2nix )
import           System.Timeout                 ( timeout )

timeoutMessage = unlines
  [ "💥 The process timed out."
  , ""
  , "  💡 You can try increasing the timeout using --timeout."
  , ""
  , "  ⛔ If the issue persists, run it again using --verbose and report the issue on Github. Sorry 😞."
  ]

main :: IO ()
main = runArgs >>= \case
  (Args i o (ProcessTimeout t) v) ->
    timeout (t * 1000000) (dconf2nix i o v) >>= \case
      Just _  -> putStrLn "🚀 Successfully Nixified! ❄️"
      Nothing -> error timeoutMessage
