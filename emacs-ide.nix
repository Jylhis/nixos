{
  lib,
  emacsWithPackagesFromUsePackage,
  emacs,
  terraform-ls,
  clang-tools,
  gcc14,
  gdb,
  rr,
  pkg-config,
  rtags,
  cppcheck,
  valgrind,
  python3,
  python-lsp-server,
  ruff,
  go,
  gopls,
  godef,
  delve,
  statix,
  nixd,
  nixfmt-rfc-style,
  sqls,
  sqlint,
  ghc,
  haskell-language-server,
  cabal-install,
  dotnet-sdk,
  csharp-ls,
  stylelint,
  yamllint,
  nasm,
  vala,
  vala-lint,
  asm-lsp,
  cmake,
  gnumake,
  eslint,
  typescript,
  jsdoc,
  nodejs_20,
  jdk,
  hadolint,
  direnv,
  nix-direnv,
  ripgrep,
  git,
  marksman,
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
        #clang_18
        clang-tools
        #lldb_18
        gcc14
        gdb
        rr
        pkg-config
        rtags
        cppcheck
        valgrind

        # Python
        python3
        python-lsp-server
        ruff

        # Golang
        go
        gopls
        godef
        delve

        # Nix
        statix
        nixd
        nixfmt-rfc-style

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
        jsdoc
        nodejs_20

        # Java
        jdk

        # Docker
        hadolint

        # CLI + other tools
        direnv
        nix-direnv
        ripgrep
        git

        # Extra LSP, linters etc.
        marksman

        terraform-ls

        # Emacs support packages
        emacs-all-the-icons-fonts
        source-code-pro
      ] ++ (prev.buildInputs or [ ]);
    }
  )
