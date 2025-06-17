{
  emacsWithPackagesFromUsePackage,
  emacs,
}:
(emacsWithPackagesFromUsePackage {
  package = emacs;
  config = ./init.org;
  defaultInitFile = true;
  extraEmacsPackages =
    epkgs: with epkgs; [
      use-package
      treesit-grammars.with-all-grammars
    ];
})