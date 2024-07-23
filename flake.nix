{
  description = "A very basic flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";

    # Nixpkgs unstable
    unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    # Hardware configuration
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    # Generate System Images
    nixos-generators.url = "github:nix-community/nixos-generators";
    nixos-generators.inputs.nixpkgs.follows = "nixpkgs";

    _1password-shell-plugins.url = "github:1Password/shell-plugins";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig = {
    extra-substituters = [
      # "https://hydra.soopy.moe"
      "https://cache.soopy.moe" # toggle these if this one doesn't work.
    ];
    extra-trusted-public-keys = ["hydra.soopy.moe:IZ/bZ1XO3IfGtq66g+C85fxU/61tgXLaJ2MlcGGXU8Q="];
  };

  outputs = {
    nixpkgs,
    flake-utils,
    nixos-hardware,
    ...
  } @ attrs:
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
    // flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      formatter = pkgs.alejandra;
      checks = {
        deadnix =
          pkgs.runCommand "lint"
          {
            buildInputs = [pkgs.deadnix];
          } ''
            set -euo pipefail
            deadnix --fail ${./.}
            mkdir $out
          '';
        statix =
          pkgs.runCommand "statix"
          {
            buildInputs = [pkgs.statix];
          } ''
            set -euo pipefail
            static check ${./.}
            mkdir $out
          '';
        shellcheck =
          pkgs.runCommand "shellcheck"
          {
            buildInputs = [pkgs.shellcheck];
          } ''
            find ${../.} -name '*.sh' ! -name "3rdparty" | xargs shellcheck -s bash
            mkdir $out
          '';
      };
    });
}
