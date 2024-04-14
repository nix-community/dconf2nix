# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "typed" = {
      at-u = mkTyped "u" 5;
      doubletime = mkUint32 7;
      empty-arr = mkArray "(dd)" [];
      empty-array-dict = mkArray "{sv}" [];
      empty-dict = mkTyped "a{sv}" {
      };
      just-empty-str = mkTyped "ms" "";
      just-str = mkTyped "ms" "hello";
      ui32 = mkUint32 7;
      var-empty-arr = mkVariant (mkArray "i" []);
    };

  };
}
