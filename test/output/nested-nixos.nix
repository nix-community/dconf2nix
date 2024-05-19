# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "org/gnome/desktop/peripherals/mouse" = {
          natural-scroll = false;
          speed = mkDouble "-0.5";
        };

        "org/gnome/desktop/peripherals/touchpad" = {
          tap-to-click = false;
          two-finger-scrolling-enabled = true;
        };

      };
    }
  ];
}
