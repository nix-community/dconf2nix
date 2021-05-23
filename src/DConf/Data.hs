module DConf.Data where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )

newtype InputFilePath = InputFilePath FilePath deriving Show
newtype OutputFilePath = OutputFilePath FilePath deriving Show
newtype ProcessTimeout = ProcessTimeout Int deriving Show

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
           | T Value Value  -- Tuple
           | TL Value Value -- Tuple within a list
           | L [Value]      -- List of values
           | EmptyList      -- Empty list (aka '@as []')
           deriving (Eq, Show)

type Header = Text
type Content = Map Key Value

data Entry = Entry
  { entryHeader :: Header
  , entryContent :: Content
  } deriving (Eq, Show)
