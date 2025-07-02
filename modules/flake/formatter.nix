# Top-level flake glue to get our configuration working
{ inputs, ... }:

{
  imports = [
    inputs.treefmt-nix.flakeModule
  ];
  perSystem =
    { pkgs, ... }:
    {
      treefmt = {
        projectRootFile = "flake.nix";

        programs.nixfmt.enable = pkgs.lib.meta.availableOn pkgs.stdenv.buildPlatform pkgs.nixfmt-rfc-style.compiler;
        programs.nixfmt.package = pkgs.nixfmt-rfc-style;
        programs.actionlint.enable = true;
        programs.deadnix.enable = true;
        programs.jsonfmt.enable = true;
        programs.shellcheck.enable = true;
        settings.formatter.shellcheck.options = [
          "-s"
          "bash"
        ];
        programs.statix.enable = true;
        programs.yamlfmt.enable = true;
        programs.cabal-fmt.enable = true;
        programs.dhall.enable = true;
        programs.gofmt.enable = true;
        programs.shfmt.enable = true;
        #	settings.formatter.ruff-check.priority = 1;
        #        settings.formatter.ruff-format.priority = 2;

      };
    };
}
