# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "gnome/desktop/interface" = {
      cursor-theme = "Yaru";
    };

    "gnome/desktop/theme" = {
      name = "Yaru";
    };

    "uk/co/ibboard/cawbird" = {
      round-avatars = false;
      startup-accounts = [ "account_name" ];
      window-geometry = {
        account_name = mkTuple [ 30 26 694 1182 ];
      };
    };

    "org/gnome/shell/extensions/bluetooth_battery_indicator" = {
      hide-indicator = true;
    };

  };
}
