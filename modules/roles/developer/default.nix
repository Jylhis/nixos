{
  config,
  options,
  lib,
  pkgs,
  nixpkgs,
  ...
}:
let
  cfg = config.jylhis.role.developer;
in
{

  imports = [ ];

  options.jylhis.role.developer = {
    enable = lib.mkEnableOption "Developer stuff";

    languages = {
      go = lib.mkEnableOption "Go lang support";
      #cc = lib.mkEnableOption "C lang support";
      cpp = lib.mkEnableOption "C++ lang support";
      nix = lib.mkEnableOption "Nix lang support";
      python = lib.mkEnableOption "Python lang support";
      haskell = lib.mkEnableOption "Haskell lang support";
      csharp = lib.mkEnableOption "CSharp lang support";
      asm = lib.mkEnableOption "ASM lang support";
      bash = lib.mkEnableOption "Bash lang support";
      javascript = lib.mkEnableOption "Javascript lang support";
      typescript = lib.mkEnableOption "Typescript lang support";
    };

  };

  config = lib.mkIf cfg.enable {

    nix.nixPath = lib.optionals cfg.languages.nix [ "nixpkgs=${nixpkgs}" ];

    environment.systemPackages =
      (lib.optionals cfg.languages.cpp [
        pkgs.cmake
        pkgs.gnumake
        pkgs.gcc
        pkgs.gdb
        pkgs.clang-tools
      ])
      ++ (lib.optionals cfg.languages.bash [
        pkgs.bash-language-server
        pkgs.shellcheck
      ])
      ++ (lib.optionals cfg.languages.go [
        pkgs.go
        pkgs.delve
        pkgs.godef
        pkgs.gopls
      ])
      ++ (lib.optionals cfg.languages.nix [
        pkgs.statix
        pkgs.nixd
        pkgs.nvd
        pkgs.nix-diff
        pkgs.nixfmt-rfc-style
        pkgs.nix-ld
        pkgs.nix-output-monitor
      ])
      ++ (lib.optionals cfg.languages.python [
        pkgs.python3
        pkgs.ruff
      ])
      ++ (lib.optionals cfg.languages.haskell [
        pkgs.ghc
        pkgs.haskell-language-server
        pkgs.cabal-install
      ])
      ++ (lib.optionals cfg.languages.csharp [
        pkgs.dotnet-sdk
        pkgs.csharp-ls
      ])
      ++ (lib.optionals cfg.languages.asm [
        pkgs.nasm
        pkgs.asm-lsp
      ]);
  };

}
