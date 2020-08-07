{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module DConfTest
  ( dconfParserTests
  )
where

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Text                      ( Text )
import           DConf                          ( dconfParser )
import           DConf.Data
import           Hedgehog
import           Text.Parsec                    ( runParser )

prop_simple_parser :: Property
prop_simple_parser = withTests (100 :: TestLimit) simpleParser

prop_file_parser :: Property
prop_file_parser = withTests (10 :: TestLimit) fileParser

simpleParser :: Property
simpleParser =
  let entries = runParser (dconfParser Normal) () "<test>" testInput
  in  property $ entries === Right [testOutput]

fileParser :: Property
fileParser = property $ do
  input   <- evalIO $ T.readFile "data/dconf.settings"
  entries <- evalEither $ runParser (dconfParser Normal) () "<test>" input
  length entries === 64

testInput :: Text
testInput = T.unlines
  [ "[ org/gnome/desktop/peripherals/mouse ]"
  , "natural-scroll=false"
  , "speed=-0.5"
  ]

testOutput :: Entry
testOutput = Entry
  { header  = "org/gnome/desktop/peripherals/mouse"
  , content = M.fromList
                [(Key "natural-scroll", B False), (Key "speed", D (-0.5))]
  }

dconfParserTests :: Group
dconfParserTests = $$(discover)
