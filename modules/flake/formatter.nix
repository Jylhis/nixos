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
        settings.formatter.jsonfmt.excludes = [
          "**/tsconfig.json"
          "**/themes/**/index.json"
        ];
        programs.shellcheck.enable = true;
        settings.formatter.shellcheck = {
          excludes = [
            "**/.envrc"
            ".envrc"
          ];
          options = [
            "-s"
            "bash"
          ];
        };
        programs.dhall.enable = true;
        programs.statix.enable = true;
        programs.yamlfmt.enable = true;
        programs.gofmt.enable = true;
        programs.shfmt.enable = true;
        programs.goimports.enable = true;
        programs.ruff-check.enable = true;
        programs.ruff-format.enable = true;
        settings.formatter.ruff-check.priority = 1;
        settings.formatter.ruff-format.priority = 2;
        programs.prettier.enable = true;
        settings.formatter.prettier = {
          excludes = [
            "**/*.min.*"
            "**/*.html"
            "**/*.json"
            "**/*-lock.json"
            ".github/**/*"
          ];
          plugins = [
            "${pkgs.prettier-plugin-go-template}/lib/node_modules/prettier-plugin-go-template/lib/index.js"
          ];
          overrides = [
            {
              files = [ "*.html" ];
              options.parser = "go-template";
            }
          ];
        };
        programs.clang-format.enable = true;
      };
    };
}
