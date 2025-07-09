{
  emacs,
}:
emacs.pkgs.withPackages (
  epkgs: with epkgs; [

    # # Tree sitter
    tree-sitter-langs
    treesit-grammars.with-all-grammars
    treesit-auto
    #
    # # QOL Nix
    pretty-sha-path
    #
    # # From config
    gcmh
    diminish

    all-the-icons
    all-the-icons-completion
    all-the-icons-dired
    wgrep
    breadcrumb
    rainbow-delimiters
    auto-dark
    super-save
    # dired-hacks-utils
    # dired-git-info
    # fd-dired
    #
    # # Org-mode
    org-appear
    mermaid-mode
    # ob-mermaid
    org-modern
    # htmlize
    #
    # # Major modes
    gnuplot
    markdown-mode
    dtrt-indent
    # google-c-style
    haskell-mode
    terraform-mode
    dockerfile-mode
    docker-compose-mode
    # web-mode
    adoc-mode
    go-mode
    nix-ts-mode
    # ruff-format
    csv-mode
    cmake-mode
    gitlab-ci-mode
    ansible
    ssh-config-mode
    # yaml-mode
    # protobuf-mode
    #
    #
    helpful
    marginalia
    vertico
    orderless
    # avy
    # goto-chg
    direnv
    consult
    embark
    embark-consult
    consult-eglot
    consult-eglot-embark
    corfu
    cape
    # eglot-tempel
    # tempel-collection
    multiple-cursors
    expand-region
    vundo
    # easy-kill
    drag-stuff
    diff-hl
    hl-todo
    magit-delta
    magit
    # magit-lfs
    # magit-todos
    treesit-fold
    dape
    # flymake-ruff
    # flymake-yamllint
    # flymake-elisp-config
    # flymake-ansible-lint
    # flymake-hadolint
    # flymake-json
    consult-flyspell
    # hledger-mode
    vterm
    sr-speedbar
  ]
)
