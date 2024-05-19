# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/tuples" = {
      n1 = mkTuple [ false (mkTuple [ "xkb" "us" ]) "nested" ];
      n2 = mkTuple [ "hi" (mkTuple [ true "us" (mkTuple [ (-787) "lvl" ]) ]) "super-nested" ];
      t0 = mkTuple [];
      t1 = mkTuple [ 5 ];
      t1-sp = mkTuple [ 5 ];
      t2 = mkTuple [ true "woman" ];
      t3 = mkTuple [ false "man" (-2) ];
      t4 = mkTuple [ "hello" 22 "world" true ];
    };

  };
}
