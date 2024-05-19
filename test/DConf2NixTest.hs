{-# LANGUAGE OverloadedStrings #-}

module DConf2NixTest
  ( dconf2nixTests
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

dconf2nixTests :: Group
dconf2nixTests = Group "Tests"
  [(fromString name, runOnce (inputTestProperty (fromString name) root)) | InputTest name root <- inputTests]

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

inputTestProperty :: FilePath -> Root -> Property
inputTestProperty name root =
  let input  = "data/" <> name <> ".settings"
      output = "output/" <> name <> ".nix"
  in  baseProperty input output root
