{
  description = "A very basic flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

    # Nixpkgs unstable
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Hardware configuration
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    # Generate System Images
    #nixos-generators.url = "github:nix-community/nixos-generators";
    #nixos-generators.inputs.nixpkgs.follows = "nixpkgs";

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs-unstable";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";

    _1password-shell-plugins.url = "github:1Password/shell-plugins";
    flake-utils.url = "github:numtide/flake-utils";

  };

  nixConfig = {
    extra-substituters = [
      # "https://hydra.soopy.moe"
      "https://cache.soopy.moe" # toggle these if this one doesn't work.
    ];
    extra-trusted-public-keys = [ "hydra.soopy.moe:IZ/bZ1XO3IfGtq66g+C85fxU/61tgXLaJ2MlcGGXU8Q=" ];
    auto-optimise-store = true;
    fallback = true;
    keep-going = true;
  };

  outputs =
    {
      nixpkgs,
      nixpkgs-unstable,
      flake-utils,
      emacs-overlay,
      nixos-hardware,
      self,
      ...
    }@attrs:
    {

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = attrs;
        modules = [
          ./configuration.nix
          nixos-hardware.nixosModules.apple-t2
        ];
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let

        pkgs = (nixpkgs.legacyPackages.${system}.extend emacs-overlay.overlay);
        unstable = nixpkgs-unstable.legacyPackages.${system};
      in
      {

        packages.emacs-ide = pkgs.callPackage ./emacs-ide.nix {
          inherit (pkgs) emacsWithPackagesFromUsePackage;
          inherit (pkgs.nodePackages) eslint jsdoc;
          inherit (pkgs.python3Packages) python-lsp-server;
          emacs = pkgs.emacs-unstable;
        };
        formatter = unstable.nixfmt-rfc-style;
        checks = {
          deadnix = pkgs.runCommand "lint" { buildInputs = [ pkgs.deadnix ]; } ''
            set -euo pipefail
            deadnix --fail ${./.}
            mkdir $out
          '';
          statix = pkgs.runCommand "statix" { buildInputs = [ pkgs.statix ]; } ''
            set -euo pipefail
            statix check ${./.}
            mkdir $out
          '';

        };
      }
    );
}
