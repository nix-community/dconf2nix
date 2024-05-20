# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "org/gnome/font-manager" = {
          compare-preview-text = "🤣";
          preview-text = "příliš žluťoučký kůň úpěl ďábelské ódy\nprilis zlutoucky kun upel dabelske ody\n◌̍◌̍ff̍ ̍čěů\n◌̎\n";
        };

      };
    }
  ];
}
