{
  compiler ? import ./ghc-version.nix,
}:

let
  packages = import ./pkgs.nix { inherit compiler; };
  inherit (packages) pkgs sources;

  toc = pkgs.resholve.mkDerivation {
    pname = "gh-md-toc";
    version = sources.github-markdown-toc.version;

    src = sources.github-markdown-toc;

    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p "$out/bin"
      cp gh-md-toc "$out/bin/gh-md-toc"
      chmod +x "$out/bin/gh-md-toc"

      runHook postInstall
    '';

    solutions = {
      default = {
        scripts = [ "bin/gh-md-toc" ];
        interpreter = "${pkgs.bash}/bin/bash";
        inputs = [
          pkgs.bash
          pkgs.coreutils
          pkgs.curl
          pkgs.gawk
          pkgs.gnugrep
          pkgs.gnused
          # which
        ];
        fix = {
          "$grepcmd" = [ "grep" "-Eo" ];
          "$SHELL" = [ "bash" ];
        };
        keep = {
          "$tool" = true;
        };
        fake = {
          external = [
            "wget"
          ];
        };
      };
    };
  };
in
pkgs.writeShellScriptBin "update-toc" ''
  ${toc}/bin/gh-md-toc --insert README.md
''
