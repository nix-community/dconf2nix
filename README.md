# dconf2nix

A convenient converter of [DConf](https://wiki.gnome.org/Projects/dconf) files to Nix, as expected by [Home Manager's dconf settings](https://rycee.gitlab.io/home-manager/options.html#opt-dconf.settings). So you can Nixify your [Gnome Shell](https://wiki.gnome.org/Projects/GnomeShell) configuration :wink:

---

Given the following `dconf` settings:

```init
[ org/gnome/desktop/peripherals/mouse ]
natural-scroll=false
speed=-0.5

[ org/gnome/desktop/peripherals/touchpad ]
tap-to-click=false
two-finger-scrolling-enabled=true

[org/gnome/desktop/input-sources]
current=uint32 0
sources=[('xkb', 'us')]
xkb-options=[' terminate:ctrl_alt_bksp ', ' lv3:ralt_switch ', ' caps:ctrl_modifier ']

[ org/gnome/desktop/screensaver ]
picture-uri=' file:///home/gvolpe/Pictures/nixos.png '
```

You will get the following output when running `dconf2nix`:

```nix
{ lib, ... }:

let
  mkTuple = lib.hm.gvariant.mkTuple;
in
{
  "org/gnome/desktop/peripherals/mouse" = {
    "natural-scroll" = false;
    "speed" = -0.5;
  };

  "org/gnome/desktop/peripherals/touchpad" = {
    "tap-to-click" = false;
    "two-finger-scrolling-enabled" = true;
  };

  "org/gnome/desktop/input-sources" = {
    "current" = "uint32 0";
    "sources" = [ (mkTuple [ "xkb" "us" ]) ];
    "xkb-options" = [ "terminate:ctrl_alt_bksp" "lv3:ralt_switch" "caps:ctrl_modifier" ];
  };

  "org/gnome/desktop/screensaver" = {
    "picture-uri" = "file:///home/gvolpe/Pictures/nixos.png";
  };

}
```

You can make changes in the UI and create a dump of your `dconf` file at any time, which you can Nixify so Home Manager can restore the next time you run `home-manager switch`. To create a dump, run the following command:

```shell
dconf dump / > dconf.settings
```

### Run

```shell
cabal new-run dconf2nix "./data/dconf.settings" "./output/dconf.nix"
```

### Installation

For now, it is only available on Github but I plan to make it available on Nixpkgs as well as in other places for easy installation.
