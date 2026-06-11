{
  description = "dev env";

  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # tracks nixpkgs unstable branch
    devshell.url = "github:numtide/devshell";
    devshell.inputs.nixpkgs.follows = "nixpkgs";
    devenv.url = "github:ramblurr/nix-devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    clj-helpers.url = "github:outskirtslabs/clojure-nix-locker-helpers";
    clj-helpers.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      devenv,
      devshell,
      clj-helpers,
      ...
    }:
    let
      package =
        pkgs:
        clj-helpers.lib.mkCljLib {
          inherit pkgs;
          name = "sfv";
          version = "0.1.0";
          src = ./.;
          jdk = pkgs.jdk21;
          extraSrcExcludes = [ "example-project" ];
          prepAliases = [
            "kaocha"
            "build"
          ];
          prefetchAliases = [ "kaocha" ];
          checkCommand = "clojure -Srepro -M:kaocha";
          gitRev = clj-helpers.lib.gitRev self;
        };
    in
    devenv.lib.mkFlake ./. {
      inherit inputs;

      withOverlays = [
        devshell.overlays.default
        devenv.overlays.default
      ];

      packages = {
        default = package;
        # regenerates ./deps-lock.json: `nix run .#locker`
        locker = pkgs: (package pkgs).locker;
      };

      devShell =
        pkgs:
        let
          jdkPackage = pkgs.jdk21;
          clojure = pkgs.clojure.override { jdk = jdkPackage; };
        in
        pkgs.devshell.mkShell {
          imports = [
            devenv.capsules.base
          ];
          packages = [
            self.packages.${pkgs.system}.locker
            clojure
            jdkPackage
            pkgs.dumbpipe
            pkgs.clojure-lsp
            pkgs.clj-kondo
            pkgs.cljfmt
            pkgs.babashka
            pkgs.git
            pkgs.wget
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
          devshell.startup.fetch-extra.text = ''
            mkdir -p extra/
            pushd extra/
            test -f rfc9651.txt || wget -q https://www.rfc-editor.org/rfc/rfc9651.txt
            test -d java-structured-fields || git clone --depth=1 https://github.com/reschke/structured-fields/ java-structured-fields
            popd
          '';
        };

      treefmtConfig = {
        programs = {
          nixfmt.enable = true;
          cljfmt.enable = true;
        };
      };
    };
}
