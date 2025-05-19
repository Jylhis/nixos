# treefmt.nix
_: {
  # Used to find the project root
  projectRootFile = "flake.nix";

  programs = {
    nixfmt.enable = true;
    actionlint.enable = true;
    deadnix.enable = true;
    jsonfmt.enable = true;
    shellcheck.enable = true;
    statix.enable = true;
    yamlfmt.enable = true;
    cabal-fmt.enable = true;
    dhall.enable = true;
    gofmt.enable = true;
    shfmt.enable = true;
  };

}
