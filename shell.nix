let
  packages = import nix/pkgs.nix {};
  inherit (packages) pkgs hp;

  drv  = hp.callCabal2nix "dconf2nix" ./. {};
in
  {
    my_project = drv;
    shell = hp.shellFor {
      name = "ghc-shell-for-dconf2nix";
      packages = p: [drv];
      buildInputs = with hp; [
        cabal-install
        haskell-language-server
        hlint
      ];
      shellHook = ''
        export NIX_GHC="$(which ghc)"
        export NIX_GHCPKG="$(which ghc-pkg)"
        export NIX_GHC_DOCDIR="$NIX_GHC/../../share/doc/ghc/html"
        export NIX_GHC_LIBDIR="$(ghc --print-libdir)"
      '';
    };
  }.shell
