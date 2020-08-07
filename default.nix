let
  pkgs = import ./pkgs.nix;
in
  pkgs.haskellPackages.callCabal2nix "dconf2nix" ./. {}
