# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "typed" = {
      at-u = mkTyped "u" 5;
      boolean = mkCast "boolean" true;
      byte = mkUchar 23;
      double = mkCast "double" (mkDouble "28.2");
      doubletime = mkUint32 (mkUint32 7);
      empty-arr = mkArray "(dd)" [];
      empty-array-dict = mkArray "{sv}" [];
      empty-dict = mkTyped "a{sv}" [
      ];
      handle = mkCast "handle" 22;
      int16 = mkInt16 30;
      int32 = mkCast "int32" 26;
      int64 = mkInt64 27;
      just-empty-str = mkTyped "ms" "";
      just-str = mkTyped "ms" "hello";
      objectpath = mkObjectpath "/org/gnome/xyz";
      string = mkCast "string" "foo";
      uint16 = mkUint16 25;
      uint32 = mkUint32 24;
      uint64 = mkUint64 21;
      var-empty-arr = mkVariant (mkArray "i" []);
    };

  };
}
