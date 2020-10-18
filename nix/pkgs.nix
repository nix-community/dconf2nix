{ compiler ? "ghc884" }:

let
  pkgs = import (
    builtins.fetchTarball {
      name   = "nixos-unstable-2020-08-15";
      url    = "https://github.com/NixOS/nixpkgs-channels/archive/96745f022835.tar.gz";
      sha256 = "1jfiaib3h6gmffwsg7d434di74x5v5pbwfifqw3l1mcisxijqm3s";
    }
  ) {};

  hp = pkgs.haskell.packages.${compiler}.override {
    overrides = newPkgs: oldPkgs: rec {
    };
  };
in
{
  pkgs = pkgs;
  hp = hp;
}
