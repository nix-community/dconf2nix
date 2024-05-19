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

-- | No need to repeat tests that do not use generator.
runOnce :: Property -> Property
runOnce = withTests 1

prop_dconf2nix :: Property
prop_dconf2nix = runOnce dconf2nix

baseProperty :: FilePath -> FilePath -> Root -> Property
baseProperty i o root = property $ do
  input  <- evalIO $ T.readFile i
  output <- evalIO $ T.readFile o
  ref    <- evalIO $ newIORef T.empty
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
      root   = Root T.empty
  in  baseProperty input output root

dconf2nixBytestring :: Property
dconf2nixBytestring =
  let input  = "data/bytestring.settings"
      output = "output/bytestring.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_bytestring :: Property
prop_dconf2nix_bytestring = runOnce dconf2nixBytestring

prop_dconf2nix_custom_root :: Property
prop_dconf2nix_custom_root = runOnce dconf2nixCustomRoot

dconf2nixCustomRoot :: Property
dconf2nixCustomRoot =
  let input  = "data/custom.settings"
      output = "output/custom.nix"
      root   = Root "ca/desrt/dconf-editor"
  in  baseProperty input output root

prop_dconf2nix_custom_nested_root :: Property
prop_dconf2nix_custom_nested_root =
  runOnce dconf2nixCustomNestedRoot

dconf2nixCustomNestedRoot :: Property
dconf2nixCustomNestedRoot =
  let input  = "data/nested.settings"
      output = "output/nested.nix"
      root   = Root "org/gnome/desktop/peripherals"
  in  baseProperty input output root

dconf2nixDict :: Property
dconf2nixDict =
  let input  = "data/dict.settings"
      output = "output/dict.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_dict :: Property
prop_dconf2nix_dict = runOnce dconf2nixDict

dconf2nixIndexer :: Property
dconf2nixIndexer =
  let input  = "data/indexer.settings"
      output = "output/indexer.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_indexer :: Property
prop_dconf2nix_indexer = runOnce dconf2nixIndexer

dconf2nixNegative :: Property
dconf2nixNegative =
  let input  = "data/numbers.settings"
      output = "output/numbers.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_negative :: Property
prop_dconf2nix_negative = runOnce dconf2nixNegative

dconf2nixJson :: Property
dconf2nixJson =
  let input  = "data/json.settings"
      output = "output/json.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_json :: Property
prop_dconf2nix_json = runOnce dconf2nixJson

dconf2nixClocks :: Property
dconf2nixClocks =
  let input  = "data/clocks.settings"
      output = "output/clocks.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_clocks :: Property
prop_dconf2nix_clocks = runOnce dconf2nixClocks

dconf2nixKeybindings :: Property
dconf2nixKeybindings =
  let input  = "data/keybindings.settings"
      output = "output/keybindings.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_keybindings :: Property
prop_dconf2nix_keybindings = runOnce dconf2nixKeybindings

dconf2nixScientificNotation :: Property
dconf2nixScientificNotation =
  let input  = "data/scientific-notation.settings"
      output = "output/scientific-notation.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_scientific_notation :: Property
prop_dconf2nix_scientific_notation =
  runOnce dconf2nixScientificNotation

dconf2nixHeaders :: Property
dconf2nixHeaders =
  let input  = "data/headers.settings"
      output = "output/headers.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_headers :: Property
prop_dconf2nix_headers =
  runOnce dconf2nixHeaders

dconf2nixTuples :: Property
dconf2nixTuples =
  let input  = "data/tuples.settings"
      output = "output/tuples.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_tuples :: Property
prop_dconf2nix_tuples =
  runOnce dconf2nixTuples

dconf2nixTyped :: Property
dconf2nixTyped =
  let input  = "data/typed.settings"
      output = "output/typed.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_typed :: Property
prop_dconf2nix_typed =
  runOnce dconf2nixTyped

dconf2nixUnicode :: Property
dconf2nixUnicode =
  let input  = "data/unicode.settings"
      output = "output/unicode.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_unicode :: Property
prop_dconf2nix_unicode =
  runOnce dconf2nixUnicode

dconf2nixVariant :: Property
dconf2nixVariant =
  let input  = "data/variant.settings"
      output = "output/variant.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_variant :: Property
prop_dconf2nix_variant =
  runOnce dconf2nixVariant

dconf2nixEmoji :: Property
dconf2nixEmoji =
  let input  = "data/emoji.settings"
      output = "output/emoji.nix"
      root   = Root T.empty
  in  baseProperty input output root

prop_dconf2nix_emoji :: Property
prop_dconf2nix_emoji =
  runOnce dconf2nixEmoji

dconf2nixTests :: Group
dconf2nixTests = $$(discover)
