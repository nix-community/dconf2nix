{
  compiler ? "ghc964",
}:

let
  nixpkgs = builtins.fetchTarball {
    name = "nixos-unstable-2024-04-11";
    url = "https://github.com/NixOS/nixpkgs/archive/1042fd8b148a9105f3c0aca3a6177fd1d9360ba5.tar.gz";
    sha256 = "0b2pcygqbx6m961mly2rfzp8xx52iik6silng85np9cvb4xxdiny";
  };
  pkgs = import nixpkgs { };

  hp = pkgs.haskell.packages.${compiler}.override {
    overrides = newPkgs: oldPkgs: { };
  };
in
{
  inherit pkgs hp;
}
