{
  description = "Jylhis personal flake";

  # TODO: deadnix
  # TODO: statix
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nix-software-center = {
      url = "github:snowfallorg/nix-software-center";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    nixos-conf-editor = {
      url = "github:snowfallorg/nixos-conf-editor";
      inputs.nixpkgs.follows = "nixpkgs";
    };

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

    stylix = {
      url = "github:danth/stylix/release-24.11";
      inputs = {
        flake-utils.follows = "flake-utils";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    tinted-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
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
      stylix,
      nixpkgs-unstable,
      deploy-rs,
      disko,
      nixos-anywhere,
      emacs-overlay,
      treefmt-nix,
      flake-utils,
      home-manager,
      nixos-hardware,
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
          overlays = [
            emacs-overlay.overlay
          ];
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
              stylix.nixosModules.stylix
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
              stylix.nixosModules.stylix
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

        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
      in
      {
        checks = {
          formatting = treefmtEval.config.build.check self;
        } // deploy-rs.lib.${system}.deployChecks self.deploy;

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

        formatter = treefmtEval.config.build.wrapper;
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

        };
      }
    );
}
