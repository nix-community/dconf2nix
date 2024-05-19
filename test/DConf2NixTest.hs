{-# LANGUAGE OverloadedStrings #-}

module DConf2NixTest
  ( dconf2nixHmTests
  , dconf2nixNixOSTests
  )
where

import           Data.IORef
import           Data.String                    ( fromString )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           DConf                          ( dconfParser )
import           DConf2Nix                      ( handler )
import           DConf.Data
import           Hedgehog
import           Text.Parsec                    ( runParser )

dconf2nixHmTests :: Group
dconf2nixHmTests = Group "Home Manager tests"
  [(fromString name, runOnce (inputTestProperty HomeManager (fromString name) root)) | InputTest name root <- inputTests]

dconf2nixNixOSTests :: Group
dconf2nixNixOSTests = Group "NixOS tests"
  [(fromString name, runOnce (inputTestProperty NixOS (fromString name) root)) | InputTest name root <- inputTests]

inputTests :: [InputTest]
inputTests =
  [ it "bytestring"
  , (it "custom") { itRoot = Root "ca/desrt/dconf-editor" }
  , it "dict"
  , it "dump"
  , it "clocks"
  , it "emoji"
  , it "headers"
  , it "indexer"
  , it "json"
  , it "keybindings"
  , (it "nested") { itRoot = Root "org/gnome/desktop/peripherals" }
  , it "numbers"
  , it "scientific-notation"
  , it "strings"
  , it "tuples"
  , it "typed"
  , it "unicode"
  , it "variant"
  ]

data InputTest = InputTest
  { itName :: String
  , itRoot :: Root
  }

it :: String -> InputTest
it name = InputTest { itName = name, itRoot = (Root T.empty) }

-- | No need to repeat tests that do not use generator.
runOnce :: Property -> Property
runOnce = withTests 1

baseProperty :: Style -> FilePath -> FilePath -> Root -> Property
baseProperty s i o root = property $ do
  input  <- evalIO $ T.readFile i
  ref    <- evalIO $ newIORef T.empty
  evalIO $ handler (writer ref) (writer ref) root s (entries input)
  result <- evalIO $ readIORef ref
  -- Uncomment to overwrite the output files.
  -- evalIO $ T.writeFile o result
  output <- evalIO $ T.readFile o
  result === output
 where
  entries = runParser (dconfParser Normal) () "<test>"
  writer ref x = modifyIORef ref (`T.append` x)

inputTestProperty :: Style -> FilePath -> Root -> Property
inputTestProperty s name root =
  let input  = "test/data/" <> name <> ".settings"
      output = "test/output/" <> name <> suffix <> ".nix"
      suffix = case s of
        HomeManager -> ""
        NixOS -> "-nixos"
  in  baseProperty s input output root
