let
  pkgs = import ./pkgs.nix;
  drv  = import ./default.nix;

  inherit (pkgs) haskellPackages;
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
