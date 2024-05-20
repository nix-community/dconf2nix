{-# LANGUAGE LambdaCase #-}

module DConf2Nix where

import           Data.Foldable                  ( traverse_ )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           DConf.Data
import           DConf                          ( dconfParser )
import qualified Nix
import           Text.Parsec.Text               ( parseFromFile )
import           Text.Parsec                    ( ParseError
                                                , runParser
                                                )

dconf2nixFile :: InputFilePath -> OutputFilePath -> Root -> Style -> Verbosity -> IO ()
dconf2nixFile (InputFilePath input) (OutputFilePath output) root s v =
  let run   = handler (T.writeFile output) (T.appendFile output) root s
      parse = parseFromFile (dconfParser v) input
  in  run =<< parse

dconf2nixStdin :: Root -> Style -> Verbosity -> IO ()
dconf2nixStdin root s v =
  let run   = handler T.putStr T.putStr root s
      parse = runParser (dconfParser v) () "<stdin>"
  in  run . parse =<< T.getContents

handler
  :: (T.Text -> IO ())
  -> (T.Text -> IO ())
  -> Root
  -> Style
  -> Either ParseError [Entry]
  -> IO ()
handler writer appender root s = \case
  Left  err -> error $ show err
  Right xs  -> do
    writer (Nix.renderHeader s)
    traverse_ (\e -> appender (unNix $ Nix.renderEntry s e root)) xs
    appender (Nix.renderFooter s)
