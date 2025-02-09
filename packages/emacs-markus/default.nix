{
  emacsWithPackagesFromUsePackage,
  emacs-nox,
}:
(emacsWithPackagesFromUsePackage {
  package = emacs-nox;
  config = ./emacs-init.el;
  defaultInitFile = true;
  alwaysEnsure = true;
  extraEmacsPackages =
    epkgs: with epkgs; [
      use-package
      treesit-grammars.with-all-grammars
      yasnippet-snippets
    ];
})
