# Custom packages, that can be defined similarly to ones from nixpkgs
# You can build them using 'nix build .#example'
pkgs: {

  # Cross compilation
  #hello-cross = nixpkgs.legacyPackages.${system}.pkgsCross.aarch64-multiplatform.hello;

  emacs-markus = pkgs.callPackage ./emacs-markus { };

  brcm-firmware = pkgs.callPackage ./brcm-firmware.nix { };
  grafana-treemap-panel = pkgs.callPackage ./grafana-treemap-panel.nix {
    inherit (pkgs.grafanaPlugins) grafanaPlugin;
  };
}
