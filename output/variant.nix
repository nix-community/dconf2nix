# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "foo" = {
      array = mkVariant [ 1 2 3 ];
      false = mkVariant false;
      in-array = [ (mkVariant 5) (mkVariant 6) ];
      int32 = mkVariant 0;
      string = mkVariant "#polari";
      true = mkVariant true;
      tuple = mkVariant (mkTuple [ (mkUint32 2) "ABC" ]);
      typed = mkVariant [];
      uint32 = mkVariant (mkUint32 2);
      variant = mkVariant (mkVariant 7);
    };

  };
}
