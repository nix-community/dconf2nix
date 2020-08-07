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

dconf2nix :: Property
dconf2nix = property $ do
  input  <- evalIO $ T.readFile "data/dconf.settings"
  output <- evalIO $ T.readFile "output/dconf.nix"
  ref    <- evalIO $ newIORef ("" :: Text)
  evalIO $ handler (writer ref) (writer ref) (entries input)
  result <- evalIO $ readIORef ref
  result === output
 where
  entries i = runParser (dconfParser Normal) () "<test>" i
  writer ref x = modifyIORef ref (`T.append` x)

dconf2nixTests :: Group
dconf2nixTests = $$(discover)
