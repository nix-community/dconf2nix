{
  packages ? import nix/pkgs.nix { inherit compiler; },
  compiler ? "ghc964",
}:

let
  inherit (packages) hp;
in
hp.callCabal2nix "dconf2nix" ./. { }
