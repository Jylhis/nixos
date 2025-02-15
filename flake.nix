{
  description = "Jylhis personal flake";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Hardware configuration
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs-stable.follows = "nixpkgs";
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager/release-24.11";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    disko = {
      url = "github:nix-community/disko/latest";
      inputs.nixpkgs.follows = "nixpkgs";

    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      emacs-overlay,
      home-manager,
      sops-nix,
      disko,
      ...
    }@attrs:

    flake-utils.lib.eachDefaultSystemPassThrough (system: {

      nixosModules = {
        common = import ./modules/common.nix;
        user-markus = import ./users/markus;
        user-sara = import ./users/sara;
        apple-hardware = import ./modules/apple-hardware.nix;
        mac-mini-2018 = import ./hardware/mac-mini-2018.nix;
        macbook-air = import ./hardware/macbook-air.nix;
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
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
              };
            }
            ./hosts/desktop

            self.nixosModules.common
            self.nixosModules.user-markus
            self.nixosModules.user-sara
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
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
              };
            }
            ./hosts/macbook-air
            self.nixosModules.common
            self.nixosModules.user-markus
            self.nixosModules.user-sara
            self.nixosModules.jyl-nix-config
            self.nixosModules.jyl-cachix
          ];
        };

        server = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            sops-nix.nixosModules.sops
            disko.nixosModules.disko
            self.nixosModules.jyl-cachix
            ./hosts/server
          ];
        };
      };

    })
    // flake-utils.lib.eachDefaultSystem (
      system:
      let

        pkgs = import nixpkgs {
          inherit system;
          allowUnfree = true;
          overlays = [
            emacs-overlay.overlay
          ];
        };
      in
      {

        packages = {

          emacs-markus = pkgs.callPackage ./packages/emacs-markus {
            emacs = pkgs.emacs-pgtk;
          };

          brcm-firmware = pkgs.callPackage ./packages/brcm-firmware.nix { };

        };

        formatter = pkgs.nixfmt-rfc-style;

        checks = {
          deadnix = pkgs.runCommand "deadnix" { buildInputs = [ pkgs.deadnix ]; } ''
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
