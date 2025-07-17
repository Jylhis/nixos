{
  emacsWithPackagesFromUsePackage,
  emacs,
}:
(emacsWithPackagesFromUsePackage {
  package = emacs;
  config = ../sources/emacs/init.el;
  defaultInitFile = true;
  extraEmacsPackages =
    epkgs: with epkgs; [
      use-package
      treesit-grammars.with-all-grammars
    ];
})
