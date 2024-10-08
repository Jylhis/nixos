{
  lib,
  emacsWithPackagesFromUsePackage,
  emacs,
  clang_18,
  clang-tools,
  lldb_18,
  gcc14,
  gdb,
  rr,
  pkg-config,

  rtags,
  cppcheck,
  valgrind,

  # Python
  python3,
  #ruff-lsp
  python-lsp-server,
  #python3Packages.python-lsp-jsonrpc
  ruff,

  # Golang
  go,
  gopls,
  godef,
  delve,

  # Elisp/lisp

  # Nix
  statix,
  nixd,
  nixfmt-rfc-style,

  # SQL
  sqls,
  sqlint,

  # Haskell
  ghc,
  haskell-language-server,
  cabal-install,

  # C# dotnet
  dotnet-sdk,
  csharp-ls,

  # HTML + CSS
  stylelint,

  # Shell + bash

  # Config languages
  yamllint,

  # Perl

  # Assembly
  nasm,
  asm-lsp,

  # Build tools
  cmake,
  gnumake,

  # Javascript & Typescript
  eslint,
  typescript,

  jsdoc,

  nodejs_20,

  # Java
  jdk,

  # Regex

  # Markdown

  # Docker
  hadolint,

  # CLI + other tools
  bat,
  direnv,
  nix-direnv,
  ripgrep,
  git,

  # Extra LSP, linters etc.
  marksman,

  # Emacs support packages
  emacs-all-the-icons-fonts,
  source-code-pro,
}:
(emacsWithPackagesFromUsePackage {
  package = emacs;
  config = ./emacs-init.el;
  defaultInitFile = true;
  alwaysEnsure = true;
  extraEmacsPackages =
    epkgs: with epkgs; [
      use-package
      treesit-grammars.with-all-grammars
    ];
}).overrideAttrs
  (
    final: prev: {
      buildInputs = [
        clang_18
        clang-tools
        lldb_18
        gcc14
        gdb
        rr
        pkg-config

        rtags
        cppcheck
        valgrind

        # Python
        python3
        #ruff-lsp
        python-lsp-server
        #python3Packages.python-lsp-jsonrpc
        ruff

        # Golang
        go
        gopls
        godef
        delve

        # Elisp/lisp

        # Nix
        statix
        nixd
        nixfmt-rfc-style

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

        # Shell + bash

        # Config languages
        yamllint

        # Perl

        # Assembly
        nasm
        asm-lsp

        # Build tools
        cmake
        gnumake

        # Javascript & Typescript
        eslint
        typescript

        jsdoc

        nodejs_20

        # Java
        jdk

        # Regex

        # Markdown

        # Docker
        hadolint

        # CLI + other tools
        bat
        direnv
        nix-direnv
        ripgrep
        git

        # Extra LSP, linters etc.
        marksman

        # Emacs support packages
        emacs-all-the-icons-fonts
        source-code-pro
      ] ++ (prev.buildInputs or [ ]);
    }
  )
