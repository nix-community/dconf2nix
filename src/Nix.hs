{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- Nix renderer for DConf entries -}
module Nix
  ( renderEntry
  , renderFooter
  , renderHeader
  )
where

import           Control.Monad                  ( (>=>) )
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
normalizeHeader h   (Root r) = normalizeRoot r <> T.replace "." "/" h

mkSpaces :: Int -> T.Text
mkSpaces = T.pack . flip replicate ' '

renderEntry :: Entry -> Root -> Nix
renderEntry (Entry h c) root =
  let header = mkSpaces 4 <> "\"" <> normalizeHeader h root <> "\" = {\n"
      body   = Map.toList c >>= \(Key k, v) ->
        T.unpack $ mkSpaces 6 <> k <> " = " <> unNix (renderValue v) <> "\n"
      close = mkSpaces 4 <> "};\n\n"
  in  Nix $ header <> T.pack body <> close

strip :: T.Text -> T.Text
strip (T.stripPrefix "'" >=> T.stripSuffix "'" -> Just t) =
  strip $ T.replace "\"" "\\\"" t
strip (T.stripPrefix "\"" >=> T.stripSuffix "\"" -> Just t) = strip t
strip t = T.strip t

renderValue :: Value -> Nix
renderValue raw = Nix $ renderValue' raw <> ";"
 where
  renderValue' (S   v) = "\"" <> strip v <> "\""
  renderValue' (B   v) = T.toLower . T.pack $ show v
  renderValue' (I   v) = T.pack $ show v
  renderValue' (D   v) = T.pack $ show v
  renderValue' (Emo v) = "\"" <> T.singleton v <> "\""
  renderValue' (I32 v) = "mkUint32 " <> T.pack (show v)
  renderValue' (I64 v) = "mkInt64 " <> T.pack (show v)
  renderValue' (T  xs) =
    let wrapNegNumber x | x < 0     = "(" <> T.pack (show x) <> ")"
                        | otherwise = T.pack $ show x
        f :: Value -> T.Text
        f (I x) = wrapNegNumber x
        f (D x) = wrapNegNumber x
        f (T v) = "(" <> renderValue' (T v) <> ")"
        f v     = renderValue' v
    in  "mkTuple [ " <> T.intercalate " " (f <$> xs) <> " ]"
  renderValue' (L xs) =
    let f :: Value -> T.Text
        f (T v) = "(" <> renderValue' (T v) <> ")"
        f v     = renderValue' v
    in  "[ " <> T.intercalate " " (f <$> xs) <> " ]"
  renderValue' EmptyList = "[]"
  renderValue' (Json v) =
    "''\n" <> mkSpaces 8 <> T.strip v <> "\n" <> mkSpaces 6 <> "''"
