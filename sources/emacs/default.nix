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
  emacs,
  pkgs,
}:
emacs.pkgs.withPackages (
  epkgs: with epkgs; [
    adoc-mode
    aidermacs
    ansible
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
    dape
    dash-docs
    devicetree-ts-mode
    diff-hl
    diminish
    direnv
    docker-compose-mode
    dockerfile-mode
    drag-stuff
    dtrt-indent
    elisp-lint
    embark
    embark-consult
    expand-region
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
    magit
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
    ox-jira
    package-lint
    pretty-sha-path
    rainbow-delimiters
    sideline
    sideline-eglot
    sideline-flymake
    sql-indent
    sr-speedbar
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
    web-server # For claude-code-ide
    websocket # For claude-code-ide
    wgrep
    yaml-mode
    zoxide
  ]
)
