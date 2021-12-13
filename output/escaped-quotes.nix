# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/shell/extensions/arcmenu" = {
      arc-menu-placement = "DTP";
      available-placement = [ false true false ];
      escaped-double-quotes = "sh -c 'notify-send \"$(date)\"'";
      simple-command = "sh -c notify-send $(date)";
      single-quotes-command = "sh -c \"notify-send $(date)\"";
    };

  };
}
