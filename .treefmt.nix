{
  projectRootFile = "flake.nix";
  programs = {
    deadnix.enable = true;
    statix.enable = true;
    shellcheck.enable = true;
    cljfmt.enable = true;
    nixfmt = {
      enable = true;
      strict = true;
    };
  };
  settings = {
    global.excludes = [
      "*.envrc"
      "extra"
      "archive"
    ];
    formatter = { };
  };
}
