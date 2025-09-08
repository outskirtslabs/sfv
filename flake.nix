{
  description = "dev env";
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # tracks nixpkgs unstable branch
    flakelight.url = "github:nix-community/flakelight";
    flakelight.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    {
      self,
      flakelight,
      ...
    }:
    flakelight ./. {

      devShell =
        pkgs:
        let
          javaVersion = "24";
          jdk = pkgs."jdk${javaVersion}";
          clojure = pkgs.clojure.override { inherit jdk; };
          libraries = [ ];
        in
        {
          packages = [
            clojure
            jdk
            pkgs.dumbpipe
            pkgs.clojure-lsp
            pkgs.clj-kondo
            pkgs.cljfmt
            pkgs.babashka
            pkgs.git
            (pkgs.writeScriptBin "run-clojure-mcp" ''
              #!/usr/bin/env bash
                set -euo pipefail
                PORT_FILE=''${1:-.nrepl-port}
                PORT=''${1:-4888}
                if [ -f "$PORT_FILE" ]; then
                PORT=$(cat ''${PORT_FILE})
                fi
                ${clojure}/bin/clojure -X:mcp/clojure :port $PORT
            '')
          ];
          env.LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libraries;
          shellHook = ''
            mkdir -p extra/
            pushd extra/
            test -f rfc9651.txt || wget -q https://www.rfc-editor.org/rfc/rfc9651.txt
            test -d java-structured-fields || git clone --depth=1 https://github.com/reschke/structured-fields/ java-structured-fields
            popd
          '';
        };

      flakelight.builtinFormatters = false;
      formatters = pkgs: {
        "*.nix" = "${pkgs.nixfmt}/bin/nixfmt";
        "*.clj" = "${pkgs.cljfmt}/bin/cljfmt fix";
      };
    };
}
