# dconf2nix

[![CI Status](https://github.com/gvolpe/dconf2nix/workflows/Haskell%20CI/badge.svg)](https://github.com/gvolpe/dconf2nix/actions)

A convenient converter of [DConf](https://wiki.gnome.org/Projects/dconf) files to Nix, as expected by [Home Manager's dconf settings](https://rycee.gitlab.io/home-manager/options.html#opt-dconf.settings). So you can Nixify your [Gnome Shell](https://wiki.gnome.org/Projects/GnomeShell) configuration :wink:

<!--ts-->
* [Benchmarks](#benchmarks)
* [Introduction](#introduction)
* [Run](#run)
  * [Custom root](#custom-root)
* [Supported types](#supported-types)
* [Gnome Shell configuration](#gnome-shell-configuration)
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
xkb-options=[' terminate:ctrl_alt_bksp ', ' lv3:ralt_switch ', ' caps:ctrl_modifier ']

[org/gnome/desktop/screensaver]
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
      natural-scroll = false;
      speed = -0.5;
    };

    "org/gnome/desktop/peripherals/touchpad" = {
      tap-to-click = false;
      two-finger-scrolling-enabled = true;
    };

    "org/gnome/desktop/input-sources" = {
      current = "uint32 0";
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

Usage: dconf2nix [-v|--version]
                 [[-r|--root ARG] [-t|--timeout ARG] [--verbose] |
                   (-i|--input ARG) (-o|--output ARG) [-r|--root ARG]
                   [-t|--timeout ARG] [--verbose]]
  Convert a dconf file into a Nix file, as expected by Home Manager.

Available options:
  -h,--help                Show this help text
  -v,--version             Show the current version
  -r,--root ARG            Custom root path. e.g.: system/locale/
  -t,--timeout ARG         Timeout in seconds for the conversion
                           process (default: 5)
  --verbose                Verbose mode (debug)
  -i,--input ARG           Path to the dconf file (input)
  -o,--output ARG          Path to the Nix output file (to be created)
  -r,--root ARG            Custom root path. e.g.: system/locale/
  -t,--timeout ARG         Timeout in seconds for the conversion
                           process (default: 5)
  --verbose                Verbose mode (debug)
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

For now, only types supported by Home Manager as specified [here](https://github.com/rycee/home-manager/blob/master/modules/lib/gvariant.nix) are supported. If there's enough interest, we might be able to work on supporting the [full specification](https://developer.gnome.org/glib/stable/gvariant-text.html).

Due to the lack of support, `dconf2nix` parses dictionaries and list of variants as simple strings to avoid failing to parse a file and retain most of the information.

### Gnome Shell configuration

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

The simplest way is to install it via `nix-env`.

```shell
nix-env -i dconf2nix
```

Or if you want to pull the latest `master`.

```shell
nix-env -i -f https://github.com/gvolpe/dconf2nix/archive/master.tar.gz
```

You could also use [Cachix](https://app.cachix.org/cache/dconf2nix) to reduce the installation time.

Alternatively, here's an overlay for the binary you can use to avoid compiling it (only for Linux-x86-64).

```nix
self: super:

rec {
  dconf2nix = super.dconf2nix.overrideAttrs (
    old: rec {
      version = "v0.0.6";

      src = builtins.fetchurl {
        url    = "https://github.com/gvolpe/dconf2nix/releases/download/${version}/dconf2nix-linux-x86-64";
        sha256 = "1bh78hfgy4wnfdq184ck5yw72szllzl5sm7a3a4y46byq0xxklcd";
      };

      phases = ["installPhase" "patchPhase"];

      installPhase = ''
        mkdir -p $out/bin
        cp $src $out/bin/dconf2nix
        chmod +x $out/bin/dconf2nix
      '';
    }
  );
}
```

Have a look at the [latest releases](https://github.com/gvolpe/dconf2nix/releases) in case the README file gets outdated.

### Troubleshooting

![error](img/error.png)

The default timeout is of 5 seconds. You can see it by running `dconf2nix --help`.

To find which section caused the error you can download [d2n_util.sh](https://github.com/broccoli5/Scripts/blob/main/bin/d2n_util.sh) (made by [Broccoli](https://github.com/broccoli5)):

```sh
curl https://raw.githubusercontent.com/broccoli5/Scripts/main/bin/d2n_util.sh > d2n_util.sh && chmod +x d2n_util.sh
```

You can then run: `dconf dump / | ./d2n_util.sh -t` to create the sections and automaticaly test them. When creating a issue include both, the sections which failed the test and the errors from "d2n.log". For more options run `./d2n_util.sh -h`

Do also consider the caveats mentioned above in the [Supported Types](#supported-types) section.

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
