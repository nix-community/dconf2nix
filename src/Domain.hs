module Domain where

import Data.Map (Map)

-- TODO: make it a newtype
type Key = String

data Value = S String
           | B Bool
           | I Int
           | I32 Int
           | I64 Int
           | D Double
           | T Value Value
           | TL Value Value -- a tuple within a list
           | L [Value]
           deriving Show

type Header  = String
type Content = Map Key Value

data Entry = Entry
  { header :: Header
  , content :: Content
  } deriving Show
