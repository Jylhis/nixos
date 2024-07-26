{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    # C/C++/Cuda
    clang
    clang-tools
    tree-sitter-grammars.tree-sitter-c
    tree-sitter-grammars.tree-sitter-cpp
    tree-sitter-grammars.tree-sitter-cuda

    # Python
    pyright
    ruff
    tree-sitter-grammars.tree-sitter-python

    # Golang
    gopls
    godef
    delve
    tree-sitter-grammars.tree-sitter-go
    tree-sitter-grammars.tree-sitter-gomod

    # Elisp/lisp
    tree-sitter-grammars.tree-sitter-elisp
    tree-sitter-grammars.tree-sitter-commonlisp

    # Nix
    nixd
    nixfmt-rfc-style
    tree-sitter-grammars.tree-sitter-nix

    # SQL
    sqls
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
    tree-sitter-grammars.tree-sitter-css
    tree-sitter-grammars.tree-sitter-scss
    tree-sitter-grammars.tree-sitter-html

    # Shell + bash
    tree-sitter-grammars.tree-sitter-bash

    # Config languages
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
    package =
      with pkgs;
      ((emacsPackagesFor emacs).emacsWithPackages (
        epkgs: with epkgs; [
          nix-mode
          markdown-mode
          markdown-toc
          direnv
          solaire-mode
          doom-themes
          treemacs
          treemacs-projectile
          treemacs-magit
          treemacs-icons-dired
          treemacs-all-the-icons
          json-mode
          json-snatcher
          flycheck
          exec-path-from-shell
          pyenv-mode
          lsp-pyright
          python-black
          blacken
          python-insert-docstring
          python
          cython-mode
          rust-mode
          go-mode
          editorconfig
          editorconfig-generate
          editorconfig-domain-specific
          editorconfig-custom-majormode
          yasnippet
          yasnippet-snippets
          magit
          system-packages
          use-package-ensure-system-package
          auto-package-update
          company
          diminish
          dashboard
          tree-sitter
          tree-sitter-langs
          tsc
          projectile
          projectile-ripgrep
          google-c-style
          modern-cpp-font-lock
          cmake-ide
          cmake-mode
          cmake-font-lock
          sphinx-doc
          sphinx-mode
          highlight-indentation
          yaml-mode
          dash
          diff-hl
          copilot
          jsonrpc
          dtrt-indent
          move-text
          consult
          helpful
          marginalia
          vertico
          which-key
          gitlab-ci-mode
          docker-compose-mode
          dockerfile-mode
          lsp-treemacs
          dap-mode
          lsp-ui
          lsp-mode
          terraform-mode
          powershell
        ]
      ));
  };
}
