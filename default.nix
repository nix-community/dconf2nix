{ packages ? import ./pkgs.nix { inherit compiler; }, compiler ? "ghc884" }:

let
  inherit (packages) hp;
in
  hp.callCabal2nix "dconf2nix" ./. {}
