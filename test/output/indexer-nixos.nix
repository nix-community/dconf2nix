# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "org/freedesktop/tracker/miner/files" = {
          index-recursive-directories = [ "&DESKTOP" "&DOCUMENTS" "&MUSIC" "&PICTURES" "&VIDEOS" "&DOWNLOAD" "/data" ];
        };

      };
    }
  ];
}
