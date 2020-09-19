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
  , "let"
  , "  mkTuple = lib.hm.gvariant.mkTuple;"
  , "in"
  , "{"
  , "  dconf.settings = {"
  ]

renderFooter :: Header
renderFooter = T.unlines ["  };", "}"]

normalizeRoot :: T.Text -> T.Text
normalizeRoot r | T.null r           = r
                | T.isSuffixOf "/" r = T.dropWhile (== '/') r
                | otherwise          = T.dropWhile (== '/') r <> "/"

normalizeHeader :: Header -> Root -> Header
normalizeHeader "/" (Root r) = T.dropWhileEnd (== '/') (normalizeRoot r) <> ""
normalizeHeader h   (Root r) = normalizeRoot r <> h

renderEntry :: Entry -> Root -> Nix
renderEntry (Entry h c) root =
  let header =
          "    \"" <> normalizeHeader h root <> "\" = {\n"
      body = Map.toList c >>= \(Key k, v) ->
        T.unpack $ "      " <> k <> " = " <> unNix (renderValue v) <> "\n"
      close = "    };\n\n"
  in  Nix $ header <> T.pack body <> close

renderValue :: Value -> Nix
renderValue raw = Nix $ renderValue' raw <> ";"
 where
  renderValue' (S   v) = "\"" <> T.strip v <> "\""
  renderValue' (B   v) = T.toLower . T.pack $ show v
  renderValue' (I   v) = T.pack $ show v
  renderValue' (D   v) = T.pack $ show v
  renderValue' (I32 v) = "\"uint32 " <> T.pack (show v) <> "\""
  renderValue' (I64 v) = "\"int64 " <> T.pack (show v) <> "\""
  renderValue' (T x y) =
    let wrapInt x | x < 0     = "(" <> T.pack (show x) <> ")"
                  | otherwise = T.pack $ show x
    in  case (x, y) of
          (I x', I y') ->
            "mkTuple [ " <> wrapInt x' <> " " <> wrapInt y' <> " ]"
          _ -> "mkTuple [ " <> renderValue' x <> " " <> renderValue' y <> " ]"
  renderValue' (TL x y) = "(" <> renderValue' (T x y) <> ")"
  renderValue' (L xs) =
    let ls = T.concat ((<> " ") <$> (renderValue' <$> xs)) in "[ " <> ls <> "]"
