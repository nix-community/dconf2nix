let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2020-06-21";
    url = "https://github.com/NixOS/nixpkgs-channels/archive/a84cbb60f02.tar.gz";
    sha256 = "04j07c98iy66hpzha7brz867dcl9lkflck43xvz09dfmlvqyzmiz";
  };

  pkgs = import nixpkgs {};

  inherit (pkgs) haskellPackages;
in
  haskellPackages.callCabal2nix "dconf2nix" ./. {}
