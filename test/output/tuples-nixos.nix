# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "org/gnome/tuples" = {
          n1 = mkTuple [ false (mkTuple [ "xkb" "us" ]) "nested" ];
          n2 = mkTuple [ "hi" (mkTuple [ true "us" (mkTuple [ (mkInt32 (-787)) "lvl" ]) ]) "super-nested" ];
          t0 = mkTuple [];
          t1 = mkTuple [ (mkInt32 5) ];
          t1-sp = mkTuple [ (mkInt32 5) ];
          t2 = mkTuple [ true "woman" ];
          t3 = mkTuple [ false "man" (mkInt32 (-2)) ];
          t4 = mkTuple [ "hello" (mkInt32 22) "world" true ];
        };

      };
    }
  ];
}
