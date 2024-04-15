# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-last-coordinates = mkTuple [ 43.68419928005759 (-79.3472) ];
    };

  };
}
