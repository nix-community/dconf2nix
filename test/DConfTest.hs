{-# LANGUAGE OverloadedStrings #-}

module DConfTest
  ( dconfParserTests
  )
where

import qualified Data.Map                      as M
import qualified Data.Text                     as T
import           Data.Text                      ( Text )
import           DConf                          ( dconfParser )
import           DConf.Data
import           Hedgehog
import           Text.Parsec                    ( runParser )

-- | No need to repeat tests that do not use generator.
runOnce :: Property -> Property
runOnce = withTests 1

simpleParser :: Property
simpleParser =
  let entries = runParser (dconfParser Normal) () "<test>" testInput
  in  property $ entries === Right [testOutput]

testInput :: Text
testInput = T.unlines
  [ "[org/gnome/desktop/peripherals/mouse]"
  , "natural-scroll=false"
  , "speed=-0.5"
  ]

testOutput :: Entry
testOutput = Entry
  { entryHeader  = "org/gnome/desktop/peripherals/mouse"
  , entryContent = M.fromList
                     [(Key "natural-scroll", B False), (Key "speed", D (-0.5))]
  }

dconfParserTests :: Group
dconfParserTests = Group "Parser tests"
  [
    ("simpleParser", runOnce simpleParser)
  ]
