# dconf2nix

[![CI Status](https://github.com/nix-commmunity/dconf2nix/workflows/Haskell%20CI/badge.svg)](https://github.com/nix-commmunity/dconf2nix/actions)

A convenient converter of [dconf](https://gitlab.gnome.org/GNOME/dconf) files to Nix, as expected by [Home Manager's dconf settings](https://rycee.gitlab.io/home-manager/options.xhtml#opt-dconf.settings). So you can Nixify your [GNOME Shell](https://gitlab.gnome.org/GNOME/gnome-shell) configuration :wink:

<!--ts-->
* [Benchmarks](#benchmarks)
* [Introduction](#introduction)
* [Run](#run)
  * [Custom root](#custom-root)
* [Supported types](#supported-types)
* [GNOME Shell configuration](#gnome-shell-configuration)
* [Installation](#installation)
* [Troubleshooting](#troubleshooting)
* [Development](#development)
<!--te-->

---

### Benchmarks

Take it with a grain of salt but on my machine it takes an average of 7.1ms to process a 349 lines configuration and generate a Nix file with 433 lines.

![benchmarks](img/benchmarks.png)

### Introduction

Given the following `dconf` settings:

```init
[org/gnome/desktop/peripherals/mouse]
natural-scroll=false
speed=-0.5

[org/gnome/desktop/peripherals/touchpad]
tap-to-click=false
two-finger-scrolling-enabled=true

[org/gnome/desktop/input-sources]
current=uint32 0
sources=[('xkb', 'us')]
xkb-options=['terminate:ctrl_alt_bksp', 'lv3:ralt_switch', 'caps:ctrl_modifier']

[org/gnome/desktop/screensaver]
picture-uri='file:///home/gvolpe/Pictures/nixos.png'
```

You will get the following output when running `dconf2nix`:

```nix
{ lib, ... }:

with lib.hm.gvariant;
{
  dconf.settings = {
    "org/gnome/desktop/peripherals/mouse" = {
      natural-scroll = false;
      speed = -0.5;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = false;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/input-sources" = {
      current = mkUint32 0;
      sources = [ (mkTuple [ "xkb" "us" ]) ];
      xkb-options = [ "terminate:ctrl_alt_bksp" "lv3:ralt_switch" "caps:ctrl_modifier" ];
    };

    "org/gnome/desktop/screensaver" = {
      picture-uri = "file:///home/gvolpe/Pictures/nixos.png";
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

The easiest way is to pipe the standard input to `dconf2nix` and expect the result in the standard output:

```shell
dconf dump / | dconf2nix > dconf.nix
```

If you have an input file instead, you can run the following command:

```shell
dconf2nix -i data/dconf.settings -o output/dconf.nix
```

Type `--help` for some more information.

```shell
dconf2nix - Nixify dconf configuration files

Usage: dconf2nix [-v|--version] [-r|--root ARG] [--verbose]
                 [(-i|--input ARG) (-o|--output ARG)]

  Convert a dconf file into a Nix file, as expected by Home Manager.

Available options:
  -h,--help                Show this help text
  -v,--version             Show the current version
  -r,--root ARG            Custom root path. e.g.: system/locale/
  --verbose                Verbose mode (debug)
  -i,--input ARG           Path to the dconf file (input)
  -o,--output ARG          Path to the Nix output file (to be created)
```

#### Custom root

By default, `dconf2nix` expects the root to be `/`. If you want to create a dump of a custom root, you can use the `--root` flag. For example:

```shell
dconf dump /system/locale/ | dconf2nix --root system/locale > dconf.nix
```

This will generate an output similar to the one below.

```nix
{
  dconf.settings = {
    "system/locale" = {
      region = "en_US.UTF-8";
    };

  };
}
```

### Supported types

With some minor spots (e.g. hexadecimal floats), the complete [GVariant text format](https://docs.gtk.org/glib/gvariant-text-format.html) is supported. But because Nix and GVariant data models are quite different, the Nix format can be a bit verbose, relying on constructor functions.

### GNOME Shell configuration

Once you have your `dconf.nix`, you can import it via Home Manager.

```nix
{
  programs.home-manager.enable = true;

  imports = [
    ./dconf.nix
  ];
}
```

If you are using the Home Manager module for NixOS you can import it like so:

```nix
{
  home-manager.users.joe = { pkgs, ... }: {
    imports = [ ./dconf.nix ];
    # ...
  };
}
```

### Installation

`dconf2nix` is available in [Nixpkgs](https://github.com/NixOS/nixpkgs) and can be installed as any other package. It can also be used without installing. For example, with flakes.

```console 
$ nix run nixpkgs#dconf2nix -- --version
<<< DCONF2NIX >>>
Version: 0.0.12
Maintainers: Nix Community
Source code: https://github.com/nix-commmunity/dconf2nix
```

To build it from source, it is recommend to use [Cachix](https://app.cachix.org/cache/dconf2nix) to reduce the compilation time.

Have a look at the [latest releases](https://github.com/nix-commmunity/dconf2nix/releases) for more information.

### Troubleshooting

Do consider the caveats mentioned above in the [Supported Types](#supported-types) section.

### Development

To compile and run the tests locally.

```shell
cabal new-configure
cabal new-run dconf2nix-tests
```

To generate the static binary.

```shell
cabal new-configure --disable-executable-dynamic --ghc-option=-optl=-static --ghc-option=-optl=-pthread
nix-build
```

If everything goes well, the binary should be under `result/bin/`.
