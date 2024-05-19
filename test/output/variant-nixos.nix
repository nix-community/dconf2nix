# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "foo" = {
          array = mkVariant [ (mkInt32 1) (mkInt32 2) (mkInt32 3) ];
          false = mkVariant false;
          in-array = [ (mkVariant (mkInt32 5)) (mkVariant (mkInt32 6)) ];
          int32 = mkVariant (mkInt32 0);
          string = mkVariant "#polari";
          true = mkVariant true;
          tuple = mkVariant (mkTuple [ (mkUint32 (mkInt32 2)) "ABC" ]);
          typed = mkVariant (mkArray "i" []);
          uint32 = mkVariant (mkUint32 (mkInt32 2));
          variant = mkVariant (mkVariant (mkInt32 7));
        };

      };
    }
  ];
}
