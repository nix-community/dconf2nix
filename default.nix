{
  packages ? import nix/pkgs.nix { inherit compiler; },
  compiler ? import nix/ghc-version.nix,
}:

let
  inherit (packages) hp;
in
hp.callCabal2nix "dconf2nix" ./. { }
