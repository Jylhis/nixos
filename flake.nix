{
  description = "Jylhis personal flake";

  # TODO: deadnix
  # TODO: statix
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    srvos = {
      url = "github:nix-community/srvos";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs.url = "github:serokell/deploy-rs";
    nixos-anywhere.url = "github:nix-community/nixos-anywhere";
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
    systems.url = "github:nix-systems/default";
    devenv = {
      url = "github:cachix/devenv";
      inputs.nixpkgs.follows = "nixpkgs";

    };
  };
  nixConfig = {
    extra-trusted-public-keys = "devenv.cachix.org-1:w1cLUi8dv3hnoSPGAuibQv+f9TZLr6cv/Hm9XgU50cw=";
    extra-substituters = "https://devenv.cachix.org";
  };
  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-unstable,
      deploy-rs,
      disko,
      nixos-anywhere,
      emacs-overlay,
      flake-utils,
      home-manager,
      nixos-hardware,
      devenv,
      sops-nix,
      srvos,
      ...
    }@attrs:

    flake-utils.lib.eachDefaultSystemPassThrough (
      system:
      let

        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
        };
      in
      {

        nixosModules = {
          rclone-sync = import ./modules/rclone-sync;
          common = import ./modules/common.nix;
          user-markus = import ./users/markus;
          user-markus-full = import ./users/markus/full.nix;
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
            specialArgs = attrs // {
              inherit unstable;
            };
            modules = [
              home-manager.nixosModules.home-manager
              {
                home-manager = {
                  useGlobalPkgs = true;
                  useUserPackages = true;
                };
              }
              ./hosts/desktop
              nixos-hardware.nixosModules.common-gpu-amd
              self.nixosModules.common
              self.nixosModules.user-markus-full
              self.nixosModules.user-sara
              self.nixosModules.jyl-nix-config
              self.nixosModules.jyl-cachix
            ];
          };
          macbook-air = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = attrs // {
              inherit unstable;
            };
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
              self.nixosModules.user-markus-full
              self.nixosModules.user-sara
              self.nixosModules.jyl-nix-config
              self.nixosModules.jyl-cachix
            ];
          };

          server = nixpkgs-unstable.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = attrs;

            modules = [
              srvos.nixosModules.server
              srvos.nixosModules.hardware-hetzner-online-intel
              sops-nix.nixosModules.sops
              disko.nixosModules.disko
              self.nixosModules.jyl-cachix
              self.nixosModules.user-markus
              self.nixosModules.rclone-sync
              ./hosts/server
            ];
          };
        };

        deploy.nodes = {
          server = {
            hostname = "lab";
            sshUser = "root";
            autoRollback = true;
            magicRollback = true;
            profiles.system = {
              user = "root";
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.server;
            };
          };
        };

        checks = builtins.mapAttrs (_system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;

      }
    )
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        unstable = import nixpkgs-unstable {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            emacs-overlay.overlay
          ];
        };
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;

        };
      in
      {

        packages = {

          # Cross compilation
          #hello-cross = nixpkgs.legacyPackages.${system}.pkgsCross.aarch64-multiplatform.hello;

          emacs-markus = unstable.callPackage ./packages/emacs-markus {
            emacs = unstable.emacs30;
          };

          brcm-firmware = pkgs.callPackage ./packages/brcm-firmware.nix { };
          grafana-treemap-panel = pkgs.callPackage ./packages/grafana-treemap-panel.nix {
            inherit (pkgs.grafanaPlugins) grafanaPlugin;
          };

        };

        formatter = pkgs.nixfmt-rfc-style;
        apps = {
          dconf-dump = {
            type = "app";
            program = pkgs.lib.getExe (
              pkgs.writeShellApplication {
                name = "dconf-dump";
                runtimeInputs = [
                  pkgs.dconf2nix
                  pkgs.gnused
                  pkgs.nixfmt-rfc-style
                ];
                bashOptions = [
                  "errexit"
                  "pipefail"
                ];
                # sed command is due to this: https://github.com/nix-community/dconf2nix/issues/112
                text = ''
                                    if [ -z "$1" ]; then
                                        echo "Usage: nix run .#dconf-dump path/to/output-file.nix"
                                        exit 1
                                    fi

                                    dconf dump / \
                                    | sed "/mru-sources/d" \
                                    | dconf2nix \
                  		  | nixfmt > "$1"
                '';
              }
            );
          };
        };
        devShells = {
          default = pkgs.mkShellNoCC {
            buildInputs = [
              # Deployment tools
              deploy-rs.packages.${system}.deploy-rs
              nixos-anywhere.packages.${system}.nixos-anywhere
              pkgs.age
              pkgs.sops
              pkgs.ssh-to-age
              pkgs.git-agecrypt

              # Other tools
              pkgs.dconf2nix
            ];
          };

          # TODO: define devenv.root
          # dev = devenv.lib.mkShell {
          #   inherit pkgs;
          #   inputs = attrs;
          #
          #   modules = [
          #     {
          #       cachix.enable = true;
          #       cachix.pull = [
          #         "pre-commit-hooks"
          #         "jylhis-nixos"
          #       ];
          #       languages.nix.enable = true;
          #       packages = [
          #         # Deployment tools
          #         deploy-rs.packages.${system}.deploy-rs
          #         nixos-anywhere.packages.${system}.nixos-anywhere
          #         pkgs.age
          #         pkgs.sops
          #         pkgs.ssh-to-age
          #         pkgs.git-agecrypt
          #
          #         # Other tools
          #         pkgs.dconf2nix
          #       ];
          #       git-hooks.excludes = [
          #         "secrets/.*\.yaml$"
          #         "users/.*/secrets\.yaml$"
          #       ];
          #       git-hooks.hooks = {
          #         shellcheck.enable = true;
          #         check-added-large-files.enable = true;
          #         check-case-conflicts.enable = true;
          #         check-executables-have-shebangs.enable = true;
          #         check-merge-conflicts.enable = true;
          #         check-shebang-scripts-are-executable.enable = true;
          #         check-symlinks.enable = true;
          #         deadnix.enable = true;
          #         detect-private-keys.enable = true;
          #         #editorconfig-checker.enable = true;
          #         fix-byte-order-marker.enable = true;
          #         forbid-new-submodules.enable = true;
          #         nil.enable = true;
          #         nixfmt-rfc-style.enable = true;
          #         ripsecrets.enable = true;
          #         statix.enable = true;
          #         typos.enable = true;
          #       };
          #     }
          #   ];
          # };

        };

      }
    );
}
