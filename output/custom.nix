# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  dconf.settings = {
    "ca/desrt/dconf-editor/" = {
      saved-pathbar-path = "/org/gnome/desktop/input-sources/";
      saved-view = "/org/gnome/desktop/input-sources/";
      window-height = 709;
      window-is-maximized = false;
      window-width = 785;
    };

  };
}
