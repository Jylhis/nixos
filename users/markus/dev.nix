{
  pkgs,
  nixpkgs,
  ...
}:
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
    ./.
  ];

  options = {
    # Option declarations.
    # Declare what settings a user of this module can set.
    # Usually this includes a global "enable" option which defaults to false.
  };

  config = {

    # For Nixd
    nix.nixPath = [ "nixpkgs=${nixpkgs}" ];

    environment.systemPackages = with pkgs; [
      devdocs-desktop

      # CLI utils
      bat
      direnv
      nix-direnv
      ripgrep
      moreutils
      btop
      htop
      fd

      # Version control stuff
      git
      delta

      # shell
      bash-language-server
      shellcheck

      # Golang
      go
      delve
      godef
      gopls

      # Nix
      statix
      nixd
      nvd
      nixfmt-rfc-style
      nix-diff
      nix-ld
      nix-output-monitor

      # Python
      python3
      python3Packages.python-lsp-server
      python3Packages.python-lsp-ruff
      python3Packages.pylsp-rope
      python3Packages.pylsp-mypy

      ruff

      # SQL
      sqls
      sqlint

      # Haskell
      ghc
      haskell-language-server
      cabal-install

      # C# dotnet
      dotnet-sdk
      csharp-ls

      # HTML + CSS
      stylelint

      nodePackages.jsonlint
      nodePackages.vscode-json-languageserver
      yaml-language-server

      # Config languages
      yamllint

      # Assembly
      nasm
      asm-lsp

      # Build tools
      cmake
      cmake-language-server
      gnumake
      gcc
      gdb

      # Javascript & Typescript
      eslint
      typescript
      typescript-language-server
      nodePackages.jsdoc

      # Docker
      dockerfile-language-server-nodejs
      hadolint

      marksman
      # Extra stuff
      emacs-all-the-icons-fonts
      source-code-pro

      clang-tools
    ];
  };
}
