# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example'

pkgs: {
  emacs-markus = pkgs.callPackage ./emacs-markus { };
  emacs-min = pkgs.callPackage ./emacs-min { };

  brcm-firmware = pkgs.callPackage ./brcm-firmware.nix { };

}
