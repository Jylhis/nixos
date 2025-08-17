# Treefmt configuration module for multi-language code formatting.
#
# This module integrates treefmt-nix to provide consistent formatting
# across multiple programming languages including Nix, Go, Python,
# JavaScript, C/C++, Haskell, and shell scripts.
#
# Features:
# - Language-specific formatters with appropriate configurations
# - Exclusion rules for files that shouldn't be formatted
# - Priority ordering for multiple formatters on the same files
#
# Usage:
# The formatting is automatically available through `nix fmt` command
# when this module is imported in the flake.
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
        programs.terraform.enable = true;
        programs.terraform.package = pkgs.opentofu;
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
        programs.hlint.enable = true;
        programs.cabal-gild.enable = true;
        programs.cabal-fmt.enable = true;
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
