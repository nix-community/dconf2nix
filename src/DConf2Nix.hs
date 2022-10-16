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

dconf2nixFile :: InputFilePath -> OutputFilePath -> Root -> EmojiSupport -> Verbosity -> IO ()
dconf2nixFile (InputFilePath input) (OutputFilePath output) root es v =
  let run   = handler (T.writeFile output) (T.appendFile output) root
      parse = parseFromFile (dconfParser es v) input
  in  run =<< parse

dconf2nixStdin :: Root -> EmojiSupport -> Verbosity -> IO ()
dconf2nixStdin root es v =
  let run   = handler T.putStr T.putStr root
      parse = runParser (dconfParser es v) () "<stdin>"
  in  run . parse =<< T.getContents

handler
  :: (T.Text -> IO ())
  -> (T.Text -> IO ())
  -> Root
  -> Either ParseError [Entry]
  -> IO ()
handler writer appender root = \case
  Left  err -> error $ show err
  Right xs  -> do
    writer Nix.renderHeader
    traverse_ (\e -> appender (unNix $ Nix.renderEntry e root)) xs
    appender Nix.renderFooter
