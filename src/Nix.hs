{- Nix renderer of DConf entries -}
module Nix (
  renderEntry,
  renderHeader
) where

import Data.Char (toLower)
import qualified Data.Map as Map
import Domain

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
      body   = (Map.toList c) >>= (\(Key k, v) -> "    \"" <> k <> "\" = " <> unNix (renderValue v) <> "\n")
      close  = "  };\n\n"
  in  Nix $ header <> body <> close

renderValue :: Value -> Nix
renderValue raw = Nix $ renderValue' raw <> ";"
 where
  renderValue' (S v)    = "\"" <> v <> "\""
  renderValue' (B v)    = toLower <$> show v
  renderValue' (I v)    = show v
  renderValue' (D v)    = show v
  renderValue' (I32 v)  = "\"uint32 " <> show v <> "\""
  renderValue' (I64 v)  = "\"int64 " <> show v <> "\""
  renderValue' (T x y)  =
    -- mkTuple [ -1 -1 ] is illegal in Home Manager (report)
    let wrapInt x | x < 0     = "\"" <> show x <> "\""
                  | otherwise = show x
    in  case (x, y) of
      (I x', I y') -> "mkTuple [ " <> wrapInt x' <> " " <> wrapInt y' <> " ]"
      _            -> "mkTuple [ " <> renderValue' x <> " " <> renderValue' y <> " ]"
  renderValue' (TL x y) = "(" <> renderValue' (T x y) <> ")"
  renderValue' (L xs)   =
    let ls = concat ((<> " ") <$> (renderValue' <$> xs))
    in  "[ " <> ls <> "]"
