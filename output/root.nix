# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "org/gnome/desktop/peripherals/mouse" = {
      natural-scroll = false;
      speed = -0.5;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = false;
      two-finger-scrolling-enabled = true;
    };

  };
}
