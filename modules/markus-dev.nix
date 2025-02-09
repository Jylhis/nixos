{ pkgs, ... }:
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
  ];

  options = {
    # Option declarations.
    # Declare what settings a user of this module can set.
    # Usually this includes a global "enable" option which defaults to false.
  };

  config = {
    # Option definitions.
    # Define what other settings, services and resources should be active.
    # Usually these depend on whether a user of this module chose to "enable" it
    # using the "option" above.
    # Options for modules imported in "imports" can be set here.
    environment.systemPackages = with pkgs; [
      # CLI utils
      bat
      direnv
      nix-direnv
      git
      ripgrep
      moreutils

      # Golang
      go
      delve
      godef
      gopls

      # Nix
      statix
      nixd
      nixfmt-rfc-style
      nix-diff

      # Python
      python3Packages.python-lsp-server
      ruff

      # vala
      vala
      vala-lint

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

      # Config languages
      yamllint

      # Assembly
      nasm
      asm-lsp

      # Build tools
      cmake
      gnumake

      # Javascript & Typescript
      eslint
      typescript
      nodePackages.jsdoc

      # Docker
      hadolint

      # Extra stuff
      emacs-all-the-icons-fonts
      source-code-pro
    ];

  };
}
