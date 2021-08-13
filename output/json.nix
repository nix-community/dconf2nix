# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

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
