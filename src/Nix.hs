{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

{- Nix renderer for DConf entries -}
module Nix
  ( renderEntry
  , renderFooter
  , renderHeader
  )
where

import           Data.Char                      ( chr, ord )
import           Data.Function                  ( (&) )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import           Data.Word                      ( Word8 )
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

-- | Distinguishes Nix expression that consist of a self-contained term (e.g. string, list)
-- from those that need to be wrapped in parentheses to be able to pass them as function
-- arguments or list items (e.g. function application).
data NixExpr = NeedsParens T.Text | Atomic T.Text

renderValue :: Value -> Nix
renderValue raw = Nix $ unparens (renderValue' raw) <> ";"
 where
  parens :: NixExpr -> T.Text
  parens (NeedsParens v) = "(" <> v <> ")"
  parens (Atomic v) = v

  unparens :: NixExpr -> T.Text
  unparens (NeedsParens v) = v
  unparens (Atomic v) = v

  renderItem :: Value -> T.Text
  renderItem v = parens (renderValue' v)

  renderList :: [Value] -> NixExpr
  renderList [] = Atomic "[]"
  renderList xs = Atomic $ "[ " <> T.intercalate " " (renderItem <$> xs) <> " ]"

  renderValue' (S   v) = renderString v
  renderValue' (B   v) = Atomic $ T.toLower . T.pack $ show v
  renderValue' (No   ) = error "Standalone nothing not supported"
  renderValue' (I   v) = (if v < 0 then NeedsParens else Atomic) (T.pack $ show v)
  renderValue' (D   v) = NeedsParens $ "mkDouble \"" <> (T.pack $ show v) <> "\""
  renderValue' (C ty v) =
    NeedsParens $ case constrName ty of
      Just constr -> T.pack constr <> " " <> renderItem v
      -- TODO: add mkCast to h-m
      Nothing -> "mkCast " <> T.pack (show (castName ty)) <> " " <> renderItem v
  renderValue' (L  xs) = renderList xs
  renderValue' (T  xs) = NeedsParens $ "mkTuple " <> parens (renderList xs)
  -- In home-manager, @mkValue []@ emits @\@as []@
  renderValue' (Ty "as" (L [])) = renderList []
  renderValue' (Ty ('m':t) No) = NeedsParens $ "mkNothing " <> T.pack (show t)
  -- In home-manager, arrays are always typed when using @mkArray@.
  renderValue' (Ty ('a':t) (L v)) = NeedsParens $ "mkArray " <> T.pack (show t) <> " " <> parens (renderList v)
  -- TODO: add mkTyped to h-m
  renderValue' (Ty t v) = NeedsParens $ "mkTyped " <> T.pack (show t) <> " " <> renderItem v
  renderValue' (Bs  v) = NeedsParens $ "mkByteString " <> renderByteString v
  renderValue' (V   v) = NeedsParens $ "mkVariant " <> renderItem v
  renderValue' (Json v) =
    Atomic $ "''\n" <> mkSpaces 8 <> T.strip v <> "\n" <> mkSpaces 6 <> "''"
  renderValue' (R kvs) =
    Atomic $ "[\n" <> mconcat (fmap (\(k,v) -> mkSpaces 8 <> renderItem (DE k v) <> "\n") kvs) <> mkSpaces 6 <> "]"
  renderValue' (DE k v) = NeedsParens $ "mkDictionaryEntry [" <> renderItem k <> " " <> renderItem v <> "]"

renderString :: T.Text -> NixExpr
renderString text = Atomic $ "\"" <> escaped <> "\""
  where
    escaped =
      text
        & T.replace "\\" "\\\\"
        & T.replace "\n" "\\n"
        & T.replace "$" "\\$"
        & T.replace "\"" "\\\""

-- We are going to use doubled apostrophes to avoid the need to escape backslashes.
renderByteString :: [Word8] -> T.Text
renderByteString bs = "''" <> T.pack (concatMap encode bs) <> "''"
  where
    encode :: Word8 -> String
    encode b =
      case chr (fromIntegral b) of
        '\a' -> "\\a"
        '\b' -> "\\b"
        '\f' -> "\\f"
        '\n' -> "\\n"
        '\r' -> "\\r"
        '\t' -> "\\t"
        '\v' -> "\\v"
        '\\' -> "\\\\"
        -- We do not need to escape double quotes. Instead we care about dollars and apostrophes.
        -- This is overly aggressive but keeping the implementation simple for now.
        '$' -> "${\"$\"}"
        '\'' -> "${\"'\"}"
        c ->
          if c < ' ' || c >= '\127' then
            "\\" <> oct "" b
          else
            [c]

oct :: String -> Word8 -> String
oct acc 0 = acc
oct acc n = oct (chr (fromIntegral (fromIntegral (ord '0') + remainder)) : acc) quotient
  where
    (quotient, remainder) = n `divMod` 8
