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

renderHeader :: Style -> Header
renderHeader HomeManager = renderHeaderHm
renderHeader NixOS = renderHeaderNixOS

renderHeaderHm :: Header
renderHeaderHm = T.unlines
  [ "# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix"
  , "{ lib, ... }:"
  , ""
  , "with lib.hm.gvariant;"
  , ""
  , "{"
  , "  dconf.settings = {"
  ]

renderHeaderNixOS :: Header
renderHeaderNixOS = T.unlines
  [ "# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix"
  , "{ lib, ... }:"
  , ""
  , "with lib.gvariant;"
  , ""
  , "{"
  , "  programs.dconf.profiles.user.databases = ["
  , "    {"
  , "      settings = {"
  ]

renderFooter :: Style -> Header
renderFooter HomeManager = renderFooterHm
renderFooter NixOS = renderFooterNixOS

renderFooterHm :: Header
renderFooterHm = T.unlines ["  };", "}"]

renderFooterNixOS :: Header
renderFooterNixOS = T.unlines
  [ "      };"
  , "    }"
  , "  ];"
  , "}"
  ]

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

indentBase :: Style -> Int
indentBase HomeManager = 4
indentBase NixOS = 8

renderEntry :: Style -> Entry -> Root -> Nix
renderEntry s (Entry h c) root =
  let base = indentBase s
      header = mkSpaces base <> "\"" <> normalizeHeader h root <> "\" = {\n"
      body   = Map.toList c >>= \(Key k, v) ->
        T.unpack $ mkSpaces (base + 2) <> k <> " = " <> unNix (renderValue s v) <> "\n"
      close = mkSpaces base <> "};\n\n"
  in  Nix $ header <> T.pack body <> close

-- | Converts type to NixOS/home-manager constructor function name.
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

renderValue :: Style -> Value -> Nix
renderValue s raw = Nix $ unparens (renderValue' initialIndent raw) <> ";"
 where
  initialIndent = indentBase s + 2

  parens :: NixExpr -> T.Text
  parens (NeedsParens v) = "(" <> v <> ")"
  parens (Atomic v) = v

  unparens :: NixExpr -> T.Text
  unparens (NeedsParens v) = v
  unparens (Atomic v) = v

  renderItem :: Int -> Value -> T.Text
  renderItem indent v = parens (renderValue' indent v)

  renderList :: Int -> [Value] -> NixExpr
  renderList _indent [] = Atomic "[]"
  renderList indent xs = Atomic $ "[ " <> T.intercalate " " (renderItem indent <$> xs) <> " ]"

  renderValue' _indent (S   v) = renderString v
  renderValue' _indent (B   v) = Atomic $ T.toLower . T.pack $ show v
  renderValue' _indent (No   ) = error "Standalone nothing not supported"
  renderValue' _indent (I   v) =
    case s of
      HomeManager -> (if v < 0 then NeedsParens else Atomic) (T.pack $ show v)
      NixOS -> NeedsParens $ "mkInt32 " <> parens ((if v < 0 then NeedsParens else Atomic) (T.pack $ show v))
  renderValue' _indent (D   v) = NeedsParens $ "mkDouble \"" <> (T.pack $ show v) <> "\""
  renderValue' indent (C ty v) =
    NeedsParens $ case constrName ty of
      Just constr -> T.pack constr <> " " <> renderItem indent v
      -- TODO: add mkCast to h-m
      Nothing -> "mkCast " <> T.pack (show (castName ty)) <> " " <> renderItem indent v
  renderValue' indent (L  xs) = renderList indent xs
  renderValue' indent (T  xs) = NeedsParens $ "mkTuple " <> parens (renderList indent xs)
  -- In home-manager, @mkValue []@ emits @\@as []@
  renderValue' indent (Ty "as" (L [])) = renderList indent []
  renderValue' _indent (Ty ('m':t) No) = NeedsParens $ "mkNothing " <> T.pack (show t)
  -- In home-manager, arrays are always typed when using @mkArray@.
  renderValue' indent (Ty ('a':t) (L v)) = NeedsParens $ "mkArray " <> T.pack (show t) <> " " <> parens (renderList indent v)
  -- TODO: add mkTyped to h-m
  renderValue' indent (Ty t v) = NeedsParens $ "mkTyped " <> T.pack (show t) <> " " <> renderItem indent v
  renderValue' _indent (Bs  v) = NeedsParens $ "mkByteString " <> renderByteString v
  renderValue' indent (V   v) = NeedsParens $ "mkVariant " <> renderItem indent v
  renderValue' indent (Json v) =
    Atomic $ "''\n" <> mkSpaces (indent + 2) <> T.strip v <> "\n" <> mkSpaces indent <> "''"
  renderValue' indent (R kvs) =
    Atomic $ "[\n" <> mconcat (fmap (\(k,v) -> mkSpaces (indent + 2) <> renderItem (indent + 2) (DE k v) <> "\n") kvs) <> mkSpaces indent <> "]"
  renderValue' indent (DE k v) =
    case s of
      HomeManager -> NeedsParens $ "mkDictionaryEntry [" <> renderItem indent k <> " " <> renderItem indent v <> "]"
      NixOS -> NeedsParens $ "mkDictionaryEntry " <> renderItem indent k <> " " <> renderItem indent v

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
