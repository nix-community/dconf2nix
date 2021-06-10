# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "org/gnome/shell/extensions/sound-output-device-chooser" = {
      hide-on-single-device = true;
      ports-settings = ''
        {"version":2,"ports":[]}
      '';
    };

  };
}
