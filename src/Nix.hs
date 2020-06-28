{- Nix renderer of DConf entries -}
module Nix (
  renderEntry,
  renderHeader,
  trim
) where

import Data.Char (toLower)
import qualified Data.Map as Map
import Domain

type Nix = String

renderHeader :: Header
renderHeader = unlines
  [ "{ lib, ... }:"
  , ""
  , "let"
  , "  mkTuple = lib.hm.gvariant.mkTuple;"
  , "in"
  , "{"
  ]

renderEntry :: Entry -> Nix
renderEntry (Entry h c) =
  let header = "  \"" <> h <> "\" = {\n"
      body   = (Map.toList c) >>= (\(f, v) -> "    \"" <> f <> "\" = " <> renderValue v <> "\n")
      close  = "  };\n\n"
  in header <> body <> close

renderValue :: Value -> Nix
renderValue raw = renderValue' raw <> ";"
 where
  renderValue' (S v)    = "\"" <> trim (takeWhile (/= '\'') v) <> "\""
  renderValue' (B v)    = toLower <$> show v
  renderValue' (I v)    = show v
  renderValue' (I32 v)  = "\"uint32 " <> show v <> "\""
  renderValue' (I64 v)  = "\"int64 " <> show v <> "\""
  renderValue' (D v)    = show v
  renderValue' (T x y)  = case (x, y) of
    (I x', I y') -> "mkTuple [ \"" <> show x' <> "\" \"" <> show y' <> "\" ]"
    _            -> "mkTuple [ " <> renderValue' x <> " " <> renderValue' y <> " ]"
  renderValue' (TL x y) = "(" <> renderValue' (T x y) <> ")"
  renderValue' (L xs)   =
    let ls = concat ((<> " ") <$> (renderValue' <$> xs))
    in  "[ " <> ls <> "]"

-- TODO: This could be replaced by parser combinators
trim :: String -> String
trim xs =
  let trim' ys = dropWhile (== ' ') ys
  in  trim' (reverse $ trim' (reverse xs))
