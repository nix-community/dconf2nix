# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "com/github/wwmm/easyeffects/streamoutputs/bassenhancer" = {
          amount = mkDouble "-6.938893903907228e-16";
          blend = mkDouble "0.0";
          listen = true;
        };

      };
    }
  ];
}
