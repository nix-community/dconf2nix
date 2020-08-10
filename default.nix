{ pkgs ? import ./pkgs.nix }:

pkgs.haskellPackages.callCabal2nix "dconf2nix" ./. {}
