{ pkgs, ... }:
let
  emacs-setup = (
    pkgs.emacsWithPackagesFromUsePackage {
      package = pkgs.emacs;
      config = ./emacs-init.el;
      defaultInitFile = true;
    }
  );
in
{
  environment.systemPackages = with pkgs; [
    emacs-setup
    # C/C++/Cuda
    clang_18
    clang-tools
    lldb_18
    gcc14
    gdb
    rr
    tree-sitter-grammars.tree-sitter-c
    tree-sitter-grammars.tree-sitter-cpp
    tree-sitter-grammars.tree-sitter-cuda
    rtags
    cppcheck
    valgrind

    # Python
    python3
    pyright
    ruff
    tree-sitter-grammars.tree-sitter-python

    # Golang
    go
    gopls
    godef
    delve
    tree-sitter-grammars.tree-sitter-go
    tree-sitter-grammars.tree-sitter-gomod

    # Elisp/lisp
    tree-sitter-grammars.tree-sitter-elisp
    tree-sitter-grammars.tree-sitter-commonlisp

    # Nix
    statix
    nixd
    nixfmt-rfc-style
    tree-sitter-grammars.tree-sitter-nix

    # SQL
    sqls
    sqlint
    tree-sitter-grammars.tree-sitter-sql

    # Haskell
    ghc
    haskell-language-server
    cabal-install
    tree-sitter-grammars.tree-sitter-haskell

    # C# dotnet
    dotnet-sdk
    csharp-ls
    tree-sitter-grammars.tree-sitter-c-sharp

    # HTML + CSS
    stylelint
    tree-sitter-grammars.tree-sitter-css
    tree-sitter-grammars.tree-sitter-scss
    tree-sitter-grammars.tree-sitter-html

    # Shell + bash
    tree-sitter-grammars.tree-sitter-bash

    # Config languages
    yamllint
    tree-sitter-grammars.tree-sitter-yaml
    tree-sitter-grammars.tree-sitter-toml
    tree-sitter-grammars.tree-sitter-json

    # Perl
    tree-sitter-grammars.tree-sitter-perl

    # Assembly
    nasm
    asm-lsp

    # Build tools
    cmake
    gnumake
    tree-sitter-grammars.tree-sitter-make
    tree-sitter-grammars.tree-sitter-llvm
    tree-sitter-grammars.tree-sitter-cmake

    # Javascript & Typescript
    nodePackages.eslint
    typescript
    tree-sitter-grammars.tree-sitter-typescript
    nodePackages.jsdoc
    tree-sitter-grammars.tree-sitter-jsdoc
    nodejs_20
    tree-sitter-grammars.tree-sitter-javascript

    # Java
    jdk
    tree-sitter-grammars.tree-sitter-java

    # Regex
    tree-sitter-grammars.tree-sitter-regex

    # Markdown
    tree-sitter-grammars.tree-sitter-markdown

    # Docker
    hadolint
    tree-sitter-grammars.tree-sitter-dockerfile

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

  ];

  services.emacs = {
    enable = true;
    package = emacs-setup;
  };

}
