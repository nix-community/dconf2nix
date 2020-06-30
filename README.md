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
  dconf.settings = {
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
  };

}
```

It makes use of the Home Manager's [dconf.settings](https://rycee.gitlab.io/home-manager/options.html#opt-dconf.settings) key.

You can make changes in the UI and create a dump of your `dconf` file at any time, which you can Nixify so Home Manager can restore the next time you run `home-manager switch`. To create a dump, run the following command:

```shell
dconf dump / > dconf.settings
```

### Run

Once compiled and installed (`cabal new-install`), you can use it as follows:

```shell
dconf2nix -i data/dconf.settings -o output/dconf.nix
```

Type `--help` for some more information.

```shell
dconf2nix - Convert dconf files to Nix

Usage: dconf2nix (-i|--input ARG) (-o|--output ARG)
  Convert a dconf file into a Nix file, as expected by Home Manager.

Available options:
  -i,--input ARG           Path to the dconf file (input)
  -o,--output ARG          Path to the Nix output file (to be created)
  -h,--help                Show this help text
```

### Gnome Shell configuration

Once you have your `dconf.nix`, you can import it via Home Manager.


```nix
{
  programs.home-manager.enable = true;

  imports = [
    ./programs/gnome/dconf.nix
  ];
}
```

You can have a look at my [NixOS configuration files](https://github.com/gvolpe/nix-config/tree/master/nixos/home) as an example.

### Installation

For now, a binary can be downloaded from [releases](https://github.com/gvolpe/dconf2nix/releases). You can also get it via [Cachix](https://app.cachix.org/cache/dconf2nix).

#### Nix

In the future, I plan to make the binary available on Nixpkgs as well as in other places for easy installation. Until then, here's a derivation you can use:

```nix
{ stdenv, ... }:

let
  version = "v0.0.1";
in
  stdenv.mkDerivation {
    name = "dconf2nix";

    src = builtins.fetchurl {
      url    = "https://github.com/gvolpe/dconf2nix/releases/download/${version}/dconf2nix-linux-x86-64";
      sha256 = "00icz1nslj6a7cxfx2fpcpvrg6sczcnqfy1pr1585477l2n6d0sn";
    };

    phases = ["installPhase" "patchPhase"];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/dconf2nix
      chmod +x $out/bin/dconf2nix
    '';
  }
```
