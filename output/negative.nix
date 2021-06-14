# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "org/gnome/settings-daemon/plugins/color" = {
      night-light-last-coordinates = mkTuple [ 43.68419928005759 (-79.3472) ];
    };

  };
}
