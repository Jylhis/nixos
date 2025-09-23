# Custom Emacs package configuration.
#
# This package provides a customized Emacs distribution with a curated
# set of packages and configurations optimized for software development.
# The configuration uses a modular structure with separate files for
# different concerns (core, UI, completion, programming, etc.).
#
# Features:
# - Modern IDE-like features with LSP support
# - Comprehensive package collection for development
# - Modular configuration for maintainability
# - Optimized for performance and usability
{
  pkgs ? import <nixpkgs> { config.allowUnfree = true; },
  emacs ? pkgs.emacs,
}:
emacs.pkgs.withPackages (
  epkgs: with epkgs; [

    # TOOD: Package: https://github.com/flymake/flymake-elsa
    adoc-mode
    modus-themes
    sideline
    sideline-flymake
    sideline-eglot
    aidermacs
    ansible
    auth-source-1password
    auto-dark
    avy
    awk-ts-mode
    bitbake-ts-mode
    breadcrumb
    cape
    cmake-mode
    compile-multi
    compile-multi-embark
    compile-multi-nerd-icons
    consult
    consult-compile-multi
    consult-dash
    consult-eglot
    consult-eglot-embark
    consult-flyspell
    copilot
    corfu
    csv-mode
    cuda-mode
    dape
    dash-docs
    demangle-mode
    devicetree-ts-mode
    diff-hl
    diminish
    direnv
    docker-compose-mode
    dockerfile-mode
    drag-stuff
    dtrt-indent
    editorconfig
    elisp-lint
    elsa
    embark
    embark-consult
    emojify
    expand-region
    projection
    projection-multi-embark
    projection-multi
    projection-dape
    git-commit-ts-mode
    gitlab-ci-mode
    gnuplot
    go-mode
    haskell-mode
    haskell-ts-mode
    helpful
    hl-todo
    jq-ts-mode
    just-mode
    just-ts-mode
    ligature
    logview
    magit
    magit-todos
    marginalia
    markdown-mode
    mermaid-mode
    mermaid-ts-mode
    modern-cpp-font-lock
    multiple-cursors
    nerd-icons
    nerd-icons-completion
    nerd-icons-corfu
    nerd-icons-dired
    nerd-icons-ibuffer
    nix-mode
    nix-ts-mode
    orderless
    org-appear
    org-jira
    org-modern
    ox-gfm
    ox-hugo
    ox-jira
    ox-slack
    package-lint
    pkgs.sqlite # consult-dash dependency
    pretty-sha-path
    rainbow-delimiters
    smartparens
    sops
    sql-indent
    ssh-config-mode
    super-save
    terraform-mode
    tree-sitter-langs
    treesit-auto
    treesit-fold
    treesit-grammars.with-all-grammars
    vertico
    vterm
    vundo
    web-mode
    web-server # For claude-code-ide
    websocket # For claude-code-ide
    wgrep
    yaml-mode
    zoxide
  ]
)
