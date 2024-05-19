# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "ca/desrt/dconf-editor" = {
      saved-pathbar-path = "/org/gnome/desktop/input-sources/";
      saved-view = "/org/gnome/desktop/input-sources/";
      window-height = 709;
      window-is-maximized = false;
      window-width = 785;
    };

    "ca/desrt/dconf-editor/foo" = {
      services = [ "service1|wg-quick@wg0.service" "service2|wg-quick@wg1.service" ];
    };

  };
}
