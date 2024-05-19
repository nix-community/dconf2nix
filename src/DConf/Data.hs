module DConf.Data where

import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Data.Word                      ( Word8 )

newtype InputFilePath = InputFilePath FilePath deriving Show
newtype OutputFilePath = OutputFilePath FilePath deriving Show

data Verbosity = Normal | Verbose

newtype Nix = Nix { unNix :: Text } deriving Show

newtype Root = Root Text deriving (Eq, Show)

newtype Key = Key Text deriving (Eq, Ord, Show)

data Ty = TyBoolean
        | TyByte
        | TyInt16
        | TyUint16
        | TyInt32
        | TyUint32
        | TyHandle
        | TyInt64
        | TyUint64
        | TyDouble
        | TyString
        | TyObjectpath
         deriving (Eq, Show, Enum)

castName :: Ty -> String
castName TyBoolean = "boolean"
castName TyByte = "byte"
castName TyInt16 = "int16"
castName TyUint16 = "uint16"
castName TyInt32 = "int32"
castName TyUint32 = "uint32"
castName TyHandle = "handle"
castName TyInt64 = "int64"
castName TyUint64 = "uint64"
castName TyDouble = "double"
castName TyString = "string"
castName TyObjectpath = "objectpath"

data Value = S Text         -- String
           | B Bool         -- Bool
           | No             -- Nothing case of Maybe
           | I Int          -- Int
           | C Ty Value     -- Cast
           | D Double       -- Double
           | T [Value]      -- Tuple of n-arity
           | Ty String Value -- Typed value
           | L [Value]      -- List of values
           | Bs [Word8]     -- Byte string (special syntax for array of bytes)
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
