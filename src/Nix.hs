{-# LANGUAGE OverloadedStrings #-}

{- Nix renderer for DConf entries -}
module Nix
  ( renderEntry
  , renderFooter
  , renderHeader
  )
where

import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           DConf.Data

renderHeader :: Header
renderHeader = T.unlines
  [ "# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix"
  , "{ lib, ... }:"
  , ""
  , "with lib.hm.gvariant;"
  , ""
  , "{"
  , "  dconf.settings = {"
  ]

renderFooter :: Header
renderFooter = T.unlines ["  };", "}"]

normalizeRoot :: T.Text -> T.Text
normalizeRoot r | T.null r           = r
                | T.isSuffixOf "/" r = f r
                | otherwise          = f r <> "/"
  where f = T.dropWhile (== '/')

normalizeHeader :: Header -> Root -> Header
normalizeHeader "/" (Root r) = T.dropWhileEnd (== '/') (normalizeRoot r)
normalizeHeader h   (Root r) = normalizeRoot r <> h

mkSpaces :: Int -> T.Text
mkSpaces = T.pack . (flip replicate ' ')

renderEntry :: Entry -> Root -> Nix
renderEntry (Entry h c) root =
  let header = mkSpaces 4 <> "\"" <> normalizeHeader h root <> "\" = {\n"
      body   = Map.toList c >>= \(Key k, v) ->
        T.unpack $ mkSpaces 6 <> k <> " = " <> unNix (renderValue v) <> "\n"
      close = mkSpaces 4 <> "};\n\n"
  in  Nix $ header <> T.pack body <> close

renderValue :: Value -> Nix
renderValue raw = Nix $ renderValue' raw <> ";"
 where
  renderValue' (S   v) = "\"" <> T.strip v <> "\""
  renderValue' (B   v) = T.toLower . T.pack $ show v
  renderValue' (I   v) = T.pack $ show v
  renderValue' (D   v) = T.pack $ show v
  renderValue' (I32 v) = "mkUint32 " <> T.pack (show v)
  renderValue' (I64 v) = "mkInt64 " <> T.pack (show v)
  renderValue' (T x y) =
    let wrapNegNumber x | x < 0     = "(" <> T.pack (show x) <> ")"
                        | otherwise = T.pack $ show x
        mkTuple x' y' =  "mkTuple [ " <> wrapNegNumber x' <> " " <> wrapNegNumber y' <> " ]"
    in  case (x, y) of
          (I x', I y') -> mkTuple x' y'
          (D x', D y') -> mkTuple x' y'
          _ -> "mkTuple [ " <> renderValue' x <> " " <> renderValue' y <> " ]"
  renderValue' (TL x y) = "(" <> renderValue' (T x y) <> ")"
  renderValue' (L xs) =
    let ls = T.concat ((<> " ") <$> (renderValue' <$> xs)) in "[ " <> ls <> "]"
  renderValue' EmptyList = "[]"
  renderValue' (Json v) =
    "''\n" <> mkSpaces 8 <> T.strip v <> "\n" <> mkSpaces 6 <> "''"
