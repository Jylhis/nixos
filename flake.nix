{
  description = "A very basic flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    # Nixpkgs unstable
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Hardware configuration
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    emacs-overlay = {

      url = "github:nix-community/emacs-overlay";
      inputs = {

        nixpkgs.follows = "nixpkgs-unstable";
        nixpkgs-stable.follows = "nixpkgs";
      };
    };

    _1password-shell-plugins.url = "github:1Password/shell-plugins";
    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager/release-24.11";
    };

    sops-nix.url = "github:Mic92/sops-nix";

  };

  nixConfig = {
    extra-substituters = [
      "https://devenv.cachix.org"
    ];
    extra-trusted-public-keys = [
      "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw="
    ];
    auto-optimise-store = true;
    fallback = true;
    keep-going = true;
  };

  outputs =
    {
      self,
      nixpkgs,

      nixpkgs-unstable,

      flake-utils,
      emacs-overlay,
      nixos-hardware,
      home-manager,

      sops-nix,
      ...
    }@attrs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend emacs-overlay.overlay;
        unstable = nixpkgs-unstable.legacyPackages.${system};

      in
      {

        packages = {

          emacs-markus = pkgs.callPackage ./packages/emacs-markus {
            inherit (pkgs) emacsWithPackagesFromUsePackage emacs;
          };

          brcm-firmware = pkgs.callPackage ./packages/brcm-firmware.nix { };

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
    )
    // flake-utils.lib.eachDefaultSystemPassThrough (system: {

      nixosModules = {
        markus = import ./users/markus.nix;
        sara = import ./users/sara.nix;
        jyl-nix-config = import ./modules/nix-config.nix;
        jyl-cachix = import ./modules/cachix.nix;
      };

      nixosConfigurations = {
        mac-mini = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = attrs;
          modules = [
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
            }
            ./hosts/desktop
            nixos-hardware.nixosModules.common-cpu-intel
            nixos-hardware.nixosModules.common-pc
            nixos-hardware.nixosModules.common-pc-ssd
            nixos-hardware.nixosModules.apple-t2
            self.nixosModules.markus
            self.nixosModules.sara
            self.nixosModules.jyl-nix-config
            self.nixosModules.jyl-cachix
          ];
        };
        macbook-air = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = attrs;
          modules = [
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
            }
            ./hosts/macbook-air

            nixos-hardware.nixosModules.common-cpu-intel
            nixos-hardware.nixosModules.common-pc-laptop
            nixos-hardware.nixosModules.common-pc-laptop-ssd
            nixos-hardware.nixosModules.apple-t2
            self.nixosModules.markus
            self.nixosModules.sara
            self.nixosModules.jyl-nix-config
            self.nixosModules.jyl-cachix
          ];
        };

        server = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./hosts/server
            sops-nix.nixosModules.sops
          ];
        };
      };

    })

  ;
}
