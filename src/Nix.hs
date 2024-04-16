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

-- | Converts type to home-manager constructor function name.
-- | Most constructors will prefix the value with type annotation, we have to avoid those that don’t or we might get ambiguous expressions.
constrName :: Ty -> Maybe String
constrName TyObjectpath = Just "mkObjectpath"
constrName TyByte = Just "mkUchar"
constrName TyInt16 = Just "mkInt16"
constrName TyUint16 = Just "mkUint16"
-- Displays value as is.
constrName TyInt32 = Nothing
constrName TyUint32 = Just "mkUint32"
constrName TyInt64 = Just "mkInt64"
constrName TyUint64 = Just "mkUint64"
-- Displays value as is.
constrName TyDouble = Nothing
constrName _ = Nothing

renderValue :: Value -> Nix
renderValue raw = Nix $ renderValue' raw <> ";"
 where
  needsParen :: Value -> Bool
  needsParen (I x) = x < 0
  needsParen (D x) = x < 0
  needsParen (C _ _) = True
  needsParen (T _) = True
  -- will be rendered as @[]@
  needsParen (Ty "as" (L [])) = False
  needsParen (Ty _ _) = True
  needsParen (V _) = True
  needsParen (DE _ _) = True
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
  renderValue' (C ty v) =
    case constrName ty of
      Just constr -> T.pack constr <> " " <> renderItem v
      -- TODO: add mkCast to h-m
      Nothing -> "mkCast " <> T.pack (show (castName ty)) <> " " <> renderItem v
  renderValue' (L  xs) = renderList xs
  renderValue' (T  xs) = "mkTuple " <> renderList xs
  -- In home-manager, @mkValue []@ emits @\@as []@
  renderValue' (Ty "as" (L [])) = renderList []
  -- In home-manager, arrays are always typed when using @mkArray@.
  renderValue' (Ty ('a':t) (L v)) = "mkArray " <> T.pack (show t) <> " " <> renderList v
  -- TODO: add mkTyped to h-m
  renderValue' (Ty t v) = "mkTyped " <> T.pack (show t) <> " " <> renderItem v
  renderValue' (V   v) = "mkVariant " <> renderItem v
  renderValue' (Json v) =
    "''\n" <> mkSpaces 8 <> T.strip v <> "\n" <> mkSpaces 6 <> "''"
  renderValue' (R kvs) =
    "[\n" <> mconcat (fmap (\(k,v) -> mkSpaces 8 <> renderItem (DE k v) <> "\n") kvs) <> mkSpaces 6 <> "]"
  renderValue' (DE k v) = "mkDictionaryEntry [" <> renderItem k <> " " <> renderItem v <> "]"

renderString :: T.Text -> T.Text
renderString text = "\"" <> escaped <> "\""
  where
    escaped =
      text
        & T.replace "\\" "\\\\"
        & T.replace "\n" "\\n"
        & T.replace "$" "\\$"
        & T.replace "\"" "\\\""
