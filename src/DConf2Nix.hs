module DConf2Nix where

import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           DConf.Data
import           DConf                          ( dconfParser )
import qualified Nix
import           Text.Parsec.Text               ( parseFromFile )
import           Text.Parsec                    ( ParseError
                                                , runParser
                                                )

dconf2nixFile :: InputFilePath -> OutputFilePath -> Verbosity -> IO ()
dconf2nixFile (InputFilePath input) (OutputFilePath output) v = do
  parsed <- parseFromFile (dconfParser v) input
  handler (T.writeFile output) (T.appendFile output) parsed

dconf2nixStdin :: Verbosity -> IO ()
dconf2nixStdin v = do
  input <- T.getContents
  handler T.putStr T.putStr $ runParser (dconfParser v) () "<stdin>" input

handler
  :: (T.Text -> IO ())
  -> (T.Text -> IO ())
  -> Either ParseError [Entry]
  -> IO ()
handler writer appender parsed = do
  case parsed of
    Left  err -> error $ show err
    Right xs  -> do
      writer Nix.renderHeader
      traverse (\e -> appender (unNix $ Nix.renderEntry e)) xs
  appender Nix.renderFooter
