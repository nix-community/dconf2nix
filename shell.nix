let
  nixpkgs = builtins.fetchTarball {
    name   = "nixos-unstable-2020-06-21";
    url    = "https://github.com/NixOS/nixpkgs-channels/archive/a84cbb60f02.tar.gz";
    sha256 = "04j07c98iy66hpzha7brz867dcl9lkflck43xvz09dfmlvqyzmiz";
  };

  pkgs = import nixpkgs {};

  inherit (pkgs) haskellPackages;
  drv = haskellPackages.callCabal2nix "dconf2nix" ./. {};
in
  {
    my_project = drv;
    shell = haskellPackages.shellFor {
      name = "ghc-shell-for-dconf2nix";
      packages = p: [drv];
      buildInputs = with haskellPackages; [ brittany hlint cabal-install ];
      shellHook = ''
        export NIX_GHC="$(which ghc)"
        export NIX_GHCPKG="$(which ghc-pkg)"
        export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
        export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
      '';
    };
  }.shell
