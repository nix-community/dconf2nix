{ compiler ? "ghc964" }:

let
  pkgs = import (
    builtins.fetchTarball {
      name = "nixos-unstable-2024-04-11";
      url = "https://github.com/NixOS/nixpkgs/archive/1042fd8b148a9105f3c0aca3a6177fd1d9360ba5.tar.gz";
      sha256 = "0b2pcygqbx6m961mly2rfzp8xx52iik6silng85np9cvb4xxdiny";
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
