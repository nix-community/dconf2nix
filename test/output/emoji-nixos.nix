# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "org/gnome/Characters" = {
          emoji-in-double-quotes = [ "🔥" ];
          recent-characters = [ "💡" ];
          some-other-character = [ "🤓" ];
        };

      };
    }
  ];
}
