# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/evince" = {
      document-directory = "@ms 'file:///home/user/Downloads'";
    };

    "empty/double/quotes" = {
      foo-bar = "";
    };

    "empty/single/quotes" = {
      foo-bar = "";
    };

  };
}
