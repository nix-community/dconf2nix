{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module DConf2NixTest
  ( dconf2nixTests
  )
where

import           Data.IORef
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           DConf                          ( dconfParser )
import           DConf2Nix                      ( handler )
import           DConf.Data
import           Hedgehog
import           Text.Parsec                    ( runParser )

prop_dconf2nix :: Property
prop_dconf2nix = withTests (10 :: TestLimit) dconf2nix

baseProperty' :: FilePath -> FilePath -> Root -> Property
baseProperty' i o root = baseProperty i o root Disabled

baseProperty :: FilePath -> FilePath -> Root -> EmojiSupport -> Property
baseProperty i o root es = property $ do
  input  <- evalIO $ T.readFile i
  output <- evalIO $ T.readFile o
  ref    <- evalIO $ newIORef T.empty
  evalIO $ handler (writer ref) (writer ref) root (entries input)
  result <- evalIO $ readIORef ref
  result === output
 where
  entries = runParser (dconfParser es Normal) () "<test>"
  writer ref x = modifyIORef ref (`T.append` x)

dconf2nix :: Property
dconf2nix =
  let input  = "data/dconf.settings"
      output = "output/dconf.nix"
      root   = Root T.empty
  in  baseProperty' input output root

prop_dconf2nix_custom_root :: Property
prop_dconf2nix_custom_root = withTests (10 :: TestLimit) dconf2nixCustomRoot

dconf2nixCustomRoot :: Property
dconf2nixCustomRoot =
  let input  = "data/custom.settings"
      output = "output/custom.nix"
      root   = Root "ca/desrt/dconf-editor"
  in  baseProperty' input output root

prop_dconf2nix_custom_nested_root :: Property
prop_dconf2nix_custom_nested_root =
  withTests (10 :: TestLimit) dconf2nixCustomNestedRoot

dconf2nixCustomNestedRoot :: Property
dconf2nixCustomNestedRoot =
  let input  = "data/nested.settings"
      output = "output/nested.nix"
      root   = Root "org/gnome/desktop/peripherals"
  in  baseProperty' input output root

dconf2nixIndexer :: Property
dconf2nixIndexer =
  let input  = "data/indexer.settings"
      output = "output/indexer.nix"
      root   = Root T.empty
  in  baseProperty' input output root

prop_dconf2nix_indexer :: Property
prop_dconf2nix_indexer = withTests (10 :: TestLimit) dconf2nixIndexer

dconf2nixNegative :: Property
dconf2nixNegative =
  let input  = "data/negative.settings"
      output = "output/negative.nix"
      root   = Root T.empty
  in  baseProperty' input output root

prop_dconf2nix_negative :: Property
prop_dconf2nix_negative = withTests (10 :: TestLimit) dconf2nixNegative

dconf2nixJson :: Property
dconf2nixJson =
  let input  = "data/json.settings"
      output = "output/json.nix"
      root   = Root T.empty
  in  baseProperty' input output root

prop_dconf2nix_json :: Property
prop_dconf2nix_json = withTests (10 :: TestLimit) dconf2nixJson

dconf2nixClocks :: Property
dconf2nixClocks =
  let input  = "data/clocks.settings"
      output = "output/clocks.nix"
      root   = Root T.empty
  in  baseProperty' input output root

prop_dconf2nix_clocks :: Property
prop_dconf2nix_clocks = withTests (10 :: TestLimit) dconf2nixClocks

dconf2nixKeybindings :: Property
dconf2nixKeybindings =
  let input  = "data/keybindings.settings"
      output = "output/keybindings.nix"
      root   = Root T.empty
  in  baseProperty' input output root

prop_dconf2nix_keybindings :: Property
prop_dconf2nix_keybindings = withTests (10 :: TestLimit) dconf2nixKeybindings

dconf2nixScientificNotation :: Property
dconf2nixScientificNotation =
  let input  = "data/scientific-notation.settings"
      output = "output/scientific-notation.nix"
      root   = Root T.empty
  in  baseProperty' input output root

prop_dconf2nix_scientific_notation :: Property
prop_dconf2nix_scientific_notation =
  withTests (10 :: TestLimit) dconf2nixScientificNotation

dconf2nixHeaders :: Property
dconf2nixHeaders =
  let input  = "data/headers.settings"
      output = "output/headers.nix"
      root   = Root T.empty
  in  baseProperty' input output root

prop_dconf2nix_headers :: Property
prop_dconf2nix_headers =
  withTests (10 :: TestLimit) dconf2nixHeaders

dconf2nixEmoji :: Property
dconf2nixEmoji =
  let input  = "data/emoji.settings"
      output = "output/emoji.nix"
      root   = Root T.empty
  in  baseProperty input output root Enabled

prop_dconf2nix_emoji :: Property
prop_dconf2nix_emoji =
  withTests (10 :: TestLimit) dconf2nixEmoji

dconf2nixTests :: Group
dconf2nixTests = $$(discover)
