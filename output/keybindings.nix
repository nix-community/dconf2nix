# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom2" = {
      binding = "<Super>space";
      command = "gnome-terminal -e \"vim --cmd startinsert\"";
      name = "Launch scratchpad";
    };

  };
}
