{
  packages ? import ./pkgs.nix { inherit compiler; },
  compiler ? import ./ghc-version.nix,
}:

let
  inherit (packages) pkgs;
in
pkgs.mkShell {
  buildInputs = [
    pkgs.nix-build-uncached
  ];
}
