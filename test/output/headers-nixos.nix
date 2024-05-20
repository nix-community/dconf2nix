# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "gnome/desktop/interface" = {
          cursor-theme = "Yaru";
        };

        "gnome/desktop/theme" = {
          name = "Yaru";
        };

        "uk/co/ibboard/cawbird" = {
          round-avatars = false;
          startup-accounts = [ "account_name" ];
          window-geometry = [
            (mkDictionaryEntry "account_name" (mkTuple [ (mkInt32 30) (mkInt32 26) (mkInt32 694) (mkInt32 1182) ]))
          ];
        };

        "org/gnome/shell/extensions/bluetooth_battery_indicator" = {
          hide-indicator = true;
        };

      };
    }
  ];
}
