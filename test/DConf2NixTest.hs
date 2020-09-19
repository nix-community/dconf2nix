{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module DConf2NixTest
  ( dconf2nixTests
  )
where

import           Data.IORef
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text )
import           DConf                          ( dconfParser )
import           DConf2Nix                      ( handler )
import           DConf.Data
import           Hedgehog
import           Text.Parsec                    ( runParser )

prop_dconf2nix :: Property
prop_dconf2nix = withTests (10 :: TestLimit) dconf2nix

baseProperty :: FilePath -> FilePath -> Root -> Property
baseProperty i o root = property $ do
  input  <- evalIO $ T.readFile i
  output <- evalIO $ T.readFile o
  ref    <- evalIO $ newIORef ("" :: Text)
  evalIO $ handler (writer ref) (writer ref) root (entries input)
  result <- evalIO $ readIORef ref
  result === output
 where
  entries = runParser (dconfParser Normal) () "<test>"
  writer ref x = modifyIORef ref (`T.append` x)

dconf2nix :: Property
dconf2nix =
  let input  = "data/dconf.settings"
      output = "output/dconf.nix"
      root   = Root ""
  in  baseProperty input output root

prop_dconf2nix_custom_root :: Property
prop_dconf2nix_custom_root = withTests (10 :: TestLimit) dconf2nixCustomRoot

dconf2nixCustomRoot :: Property
dconf2nixCustomRoot =
  let input  = "data/custom.settings"
      output = "output/custom.nix"
      root   = Root "ca/desrt/dconf-editor"
  in  baseProperty input output root

dconf2nixTests :: Group
dconf2nixTests = $$(discover)
