{ packages ? import ./pkgs.nix { inherit compiler; }, compiler ? "ghc883" }:

let
  inherit (packages) hp;
in
  hp.callCabal2nix "dconf2nix" ./. {}
