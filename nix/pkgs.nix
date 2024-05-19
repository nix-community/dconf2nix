{
  compiler ? "ghc964",
}:

let
  sources = import ../npins;
  pkgs = import sources.nixpkgs { };

  hp = pkgs.haskell.packages.${compiler}.override {
    overrides = newPkgs: oldPkgs: { };
  };
in
{
  inherit pkgs hp sources;
}
