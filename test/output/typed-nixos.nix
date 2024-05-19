# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "typed" = {
          at-u = mkTyped "u" (mkInt32 5);
          boolean = mkCast "boolean" true;
          byte = mkUchar (mkInt32 23);
          double = mkCast "double" (mkDouble "28.2");
          doubletime = mkUint32 (mkUint32 (mkInt32 7));
          empty-arr = mkArray "(dd)" [];
          empty-array-dict = mkArray "{sv}" [];
          empty-dict = mkTyped "a{sv}" [
          ];
          handle = mkCast "handle" (mkInt32 22);
          int16 = mkInt16 (mkInt32 30);
          int32 = mkCast "int32" (mkInt32 26);
          int64 = mkInt64 (mkInt32 27);
          just-empty-str = mkTyped "ms" "";
          just-str = mkTyped "ms" "hello";
          objectpath = mkObjectpath "/org/gnome/xyz";
          string = mkCast "string" "foo";
          uint16 = mkUint16 (mkInt32 25);
          uint32 = mkUint32 (mkInt32 24);
          uint64 = mkUint64 (mkInt32 21);
          var-empty-arr = mkVariant (mkArray "i" []);
        };

      };
    }
  ];
}
