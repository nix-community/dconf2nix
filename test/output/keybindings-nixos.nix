# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
          binding = "<Super>space";
          command = "gnome-terminal -e \"vim --cmd startinsert\"";
          name = "Launch scratchpad";
        };

      };
    }
  ];
}
