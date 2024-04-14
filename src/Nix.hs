{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- Nix renderer for DConf entries -}
module Nix
  ( renderEntry
  , renderFooter
  , renderHeader
  )
where

import           Data.Function                  ( (&) )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           DConf.Data

renderHeader :: Header
renderHeader = T.unlines
  [ "# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix"
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

renderValue :: Value -> Nix
renderValue raw = Nix $ renderValue' raw <> ";"
 where
  needsParen :: Value -> Bool
  needsParen (I x) = x < 0
  needsParen (D x) = x < 0
  needsParen (I32 _) = True
  needsParen (T _) = True
  needsParen (V _) = True
  needsParen _ = False

  renderItem :: Value -> T.Text
  renderItem v | needsParen v = "(" <> renderValue' v <> ")"
  renderItem v = renderValue' v

  renderList :: [Value] -> T.Text
  renderList [] = "[]"
  renderList xs = let
    in "[ " <> T.intercalate " " (renderItem <$> xs) <> " ]"

  renderValue' (S   v) = renderString v
  renderValue' (B   v) = T.toLower . T.pack $ show v
  renderValue' (I   v) = T.pack $ show v
  renderValue' (D   v) = T.pack $ show v
  renderValue' (I32 v) = "mkUint32 " <> T.pack (show v)
  renderValue' (I64 v) = "mkInt64 " <> T.pack (show v)
  renderValue' (L  xs) = renderList xs
  renderValue' (T  xs) = "mkTuple " <> renderList xs
  renderValue' (V  xs) = "mkVariant " <> renderList xs
  renderValue' (Json v) =
    "''\n" <> mkSpaces 8 <> T.strip v <> "\n" <> mkSpaces 6 <> "''"
  renderValue' (R kvs) =
    "{\n" <> mconcat (fmap (\(k,v) -> mkSpaces 8 <> k <> " = " <> renderValue' v <> ";\n") kvs) <> mkSpaces 6 <> "}"

renderString :: T.Text -> T.Text
renderString text = "\"" <> escaped <> "\""
  where
    escaped =
      text
        & T.replace "\\" "\\\\"
        & T.replace "\n" "\\n"
        & T.replace "$" "\\$"
        & T.replace "\"" "\\\""
