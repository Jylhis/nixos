{
  description = "Jylhis personal flake";

  # TODO: deadnix
  # TODO: statix
  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";

    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-small.url = "github:nixos/nixpkgs/nixos-unstable-small";

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
      inputs = {

        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
      };
    };
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";

    srvos = {
      url = "github:nix-community/srvos";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {

        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";

      };

    };
    flake-parts.url = "github:hercules-ci/flake-parts";

    nixos-anywhere = {
      url = "github:nix-community/nixos-anywhere";
      inputs = {
        flake-parts.follows = "flake-parts";
        nixpkgs.follows = "nixpkgs-small";
        disko.follows = "disko";
        nixos-stable.follows = "nixpkgs";
        treefmt-nix.follows = "treefmt-nix";
      };
    };
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
        flake-compat.follows = "flake-compat";
        home-manager.follows = "home-manager";
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
      };
    };

    tinted-schemes = {
      url = "github:tinted-theming/schemes";
      flake = false;
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

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

    nil-lsp = {
      url = "github:oxalica/nil";
    };
    systems.url = "github:nix-systems/default";
    musnix = {
      url = "github:musnix/musnix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };
  outputs =
    {
      self,
      nixpkgs,
      stylix,
      nixpkgs-unstable,
      deploy-rs,
      disko,
      emacs-overlay,
      treefmt-nix,
      home-manager,
      nixos-hardware,
      sops-nix,
      srvos,
      ...
    }@attrs:
    let
      system = "x86_64-linux";

      pkgs-stable = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          emacs-overlay.overlay
          #      nixos-anywhere.overlay
        ];
      };

      pkgs-unstable = import nixpkgs-unstable {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          emacs-overlay.overlay
        ];
      };

      treefmtEval = treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix;
    in
    {
      checks = deploy-rs.lib.${system}.deployChecks self.deploy // {
        ${system} = {
          formatting = treefmtEval.config.build.check self;
        };
      };

      # TODO emacs overlay
      packages.${system} = import ./pkgs pkgs-stable;

      formatter.${system} = treefmtEval.config.build.wrapper;

      apps.${system} = import ./apps nixpkgs.legacyPackages.${system};

      devShells.${system}.default =
        let
          pkgs = pkgs-stable;
        in
        pkgs.mkShellNoCC {
          buildInputs = [
            # Deployment tools
            deploy-rs.packages.${system}.deploy-rs
            #   pkgs.nixos-anywhere
            pkgs.age
            pkgs.sops
            pkgs.ssh-to-age
            pkgs.git-agecrypt

            # Other tools
            pkgs.dconf2nix
          ];
        };

      #overlays = import ./overlays {inherit inputs;};

      nixosModules = import ./modules/nixos // (import ./users);
      #homeManagerModules = import ./modules/home-manager;

      nixosConfigurations = {
        mac-mini = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = attrs // {
            unstable = pkgs-unstable;
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
            self.nixosModules.personal-defaults
            self.nixosModules.nix-companion
            self.nixosModules.user-markus-full
            self.nixosModules.user-sara
            self.nixosModules.jyl-cachix
          ];
        };
        macbook-air = nixpkgs.lib.nixosSystem rec {
          inherit system;
          specialArgs = attrs // {
            unstable = pkgs-unstable;
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
            self.nixosModules.personal-defaults
            self.nixosModules.nix-companion
            self.nixosModules.user-markus-full
            self.nixosModules.user-sara
            self.nixosModules.jyl-cachix
          ];
        };

        server = nixpkgs-unstable.lib.nixosSystem {
          inherit system;
          specialArgs = attrs // {
            unstable = pkgs-unstable;
          };

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
            path = deploy-rs.lib.${system}.activate.nixos self.nixosConfigurations.server;
          };
        };
      };

    };
}
