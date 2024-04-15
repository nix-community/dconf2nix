module DConf.Data where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )

newtype InputFilePath = InputFilePath FilePath deriving Show
newtype OutputFilePath = OutputFilePath FilePath deriving Show

data Verbosity = Normal | Verbose

newtype Nix = Nix { unNix :: Text } deriving Show

newtype Root = Root Text deriving (Eq, Show)

newtype Key = Key Text deriving (Eq, Ord, Show)

data Value = S Text         -- String
           | B Bool         -- Bool
           | I Int          -- Int
           | I32 Int        -- Int32
           | I64 Int        -- Int64
           | D Double       -- Double
           | T [Value]      -- Tuple of n-arity
           | Ty String Value -- Typed value
           | L [Value]      -- List of values
           | V Value        -- Variant
           | R [(Value,Value)] -- Dictionary
           | DE Value Value -- Dictionary entry
           | Json Text      -- Json value
           deriving (Eq, Show)

type Header = Text
type Content = Map Key Value

data Entry = Entry
  { entryHeader :: Header
  , entryContent :: Content
  } deriving (Eq, Show)
