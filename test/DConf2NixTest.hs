{-# LANGUAGE OverloadedStrings #-}

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

dump :: Property
dump =
  let input  = "data/dconf.settings"
      output = "output/dconf.nix"
      root   = Root T.empty
  in  baseProperty input output root

bytestring :: Property
bytestring =
  let input  = "data/bytestring.settings"
      output = "output/bytestring.nix"
      root   = Root T.empty
  in  baseProperty input output root

customRoot :: Property
customRoot =
  let input  = "data/custom.settings"
      output = "output/custom.nix"
      root   = Root "ca/desrt/dconf-editor"
  in  baseProperty input output root

customNestedRoot :: Property
customNestedRoot =
  let input  = "data/nested.settings"
      output = "output/nested.nix"
      root   = Root "org/gnome/desktop/peripherals"
  in  baseProperty input output root

dict :: Property
dict =
  let input  = "data/dict.settings"
      output = "output/dict.nix"
      root   = Root T.empty
  in  baseProperty input output root

indexer :: Property
indexer =
  let input  = "data/indexer.settings"
      output = "output/indexer.nix"
      root   = Root T.empty
  in  baseProperty input output root

numbers :: Property
numbers =
  let input  = "data/numbers.settings"
      output = "output/numbers.nix"
      root   = Root T.empty
  in  baseProperty input output root

json :: Property
json =
  let input  = "data/json.settings"
      output = "output/json.nix"
      root   = Root T.empty
  in  baseProperty input output root

clocks :: Property
clocks =
  let input  = "data/clocks.settings"
      output = "output/clocks.nix"
      root   = Root T.empty
  in  baseProperty input output root

keybindings :: Property
keybindings =
  let input  = "data/keybindings.settings"
      output = "output/keybindings.nix"
      root   = Root T.empty
  in  baseProperty input output root

scientificNotation :: Property
scientificNotation =
  let input  = "data/scientific-notation.settings"
      output = "output/scientific-notation.nix"
      root   = Root T.empty
  in  baseProperty input output root

headers :: Property
headers =
  let input  = "data/headers.settings"
      output = "output/headers.nix"
      root   = Root T.empty
  in  baseProperty input output root

tuples :: Property
tuples =
  let input  = "data/tuples.settings"
      output = "output/tuples.nix"
      root   = Root T.empty
  in  baseProperty input output root

typed :: Property
typed =
  let input  = "data/typed.settings"
      output = "output/typed.nix"
      root   = Root T.empty
  in  baseProperty input output root

unicode :: Property
unicode =
  let input  = "data/unicode.settings"
      output = "output/unicode.nix"
      root   = Root T.empty
  in  baseProperty input output root

variant :: Property
variant =
  let input  = "data/variant.settings"
      output = "output/variant.nix"
      root   = Root T.empty
  in  baseProperty input output root

emoji :: Property
emoji =
  let input  = "data/emoji.settings"
      output = "output/emoji.nix"
      root   = Root T.empty
  in  baseProperty input output root

dconf2nixTests :: Group
dconf2nixTests = Group "Tests"
  [ ("bytestring", runOnce bytestring)
  , ("clocks", runOnce clocks)
  , ("customNestedRoot", runOnce customNestedRoot)
  , ("customRoot", runOnce customRoot)
  , ("dict", runOnce dict)
  , ("dump", runOnce dump)
  , ("emoji", runOnce emoji)
  , ("headers", runOnce headers)
  , ("indexer", runOnce indexer)
  , ("json", runOnce json)
  , ("keybindings", runOnce keybindings)
  , ("numbers", runOnce numbers)
  , ("scientificNotation", runOnce scientificNotation)
  , ("tuples", runOnce tuples)
  , ("typed", runOnce typed)
  , ("unicode", runOnce unicode)
  , ("variant", runOnce variant)
  ]
