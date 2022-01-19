# Generated via dconf2nix: https://github.com/gvolpe/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "org/gnome/shell/extensions/sound-output-device-chooser" = {
      hide-on-single-device = true;
      ports-settings = ''
        {"version":2,"ports":[]}
      '';
    };

    "org/gnome/shell/extensions/list-of-json" = {
      devices = [ ''
        {"_model":{},"name":"FakeName","isConnected":false,"isPaired":true,"mac":"00:16:00:29:00:83","isDefault":false,"active":true,"icon":"audio-headphones-symbolic"}
      '' ];
      foo = [ ''
        {"_model":{}}
      '' ];
    };

  };
}
