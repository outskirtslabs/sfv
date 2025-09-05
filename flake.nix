{
  description = "dev env";
  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1"; # tracks nixpkgs unstable branch
    flakelight.url = "github:nix-community/flakelight";
    flakelight.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };
  outputs =
    {
      self,
      flakelight,
      treefmt-nix,
      ...
    }:
    let
      treefmtEval = pkgs: treefmt-nix.lib.evalModule pkgs ./.treefmt.nix;
    in
    flakelight ./. {

      devShell =
        pkgs:
        let
          javaVersion = "24";
          jdk = pkgs."jdk${javaVersion}";
          clojure = pkgs.clojure.override { inherit jdk; };
          libraries = [];
        in
        {
          packages = [
            clojure
            jdk
            pkgs.clojure-lsp
            pkgs.clj-kondo
            pkgs.cljfmt
            pkgs.babashka
            pkgs.git
          ];
          env.LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath libraries;
        };

      flakelight.builtinFormatters = false;
      formatter =
        pkgs:
        let
          trfmt = treefmtEval pkgs;
        in
        trfmt.config.build.wrapper;
    };
}
