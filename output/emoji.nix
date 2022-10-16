# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/Characters" = {
      recent-characters = [ "💡" ];
      some-other-character = [ "🤓" ];
    };

  };
}
