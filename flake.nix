{
  description = "Jylhis personal flake";

  inputs = {

    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-compat.url = "github:nix-community/flake-compat";

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs-stable.follows = "nixpkgs";
        nixpkgs.follows = "nixpkgs-unstable";
      };
    };

    stylix = {
      url = "github:danth/stylix/release-25.05";
      inputs = {
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
      url = "github:nix-community/home-manager/release-25.05";
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

  };
  outputs =
    {
      self,
      nixpkgs,
      emacs-overlay,
      treefmt-nix,
      ...
    }:
    let
      system = "x86_64-linux";

      pkgs-stable = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          emacs-overlay.overlay
          self.overlays.by-name
          self.overlays.additions
        ];
      };

      treefmtEval = treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix;
    in
    {
      checks.${system}.formatting = treefmtEval.config.build.check self;

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
            #   pkgs.nixos-anywhere
            pkgs.age
            pkgs.sops
            pkgs.ssh-to-age
            pkgs.git-agecrypt

            # Other tools
            pkgs.dconf2nix

            # SEC
            pkgs.vulnix
            pkgs.sbomnix

          ];
        };

      overlays = import ./overlays { inherit (nixpkgs) lib; };
      nixosModules = import ./modules;
      #homeManagerModules = import ./modules/home-manager;
    };
}
