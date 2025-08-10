# Development shell configuration for the j10s monorepo.
#
# Provides a development environment with all necessary tools for
# working with this Nix configuration including deployment tools,
# secrets management, and security scanning utilities.
#
# Available tools:
# - just: Command runner for common tasks
# - nixd: Nix language server
# - deploy-rs: NixOS deployment tool
# - age/sops: Secret encryption and management
# - vulnix/sbomnix: Security scanning tools
#
# Usage:
# Enter the development shell with `nix develop` or `just dev`
{
  perSystem =
    { pkgs, ... }:
    {
      devShells.default = pkgs.mkShellNoCC {
        name = "j10s-devshell";
        meta.description = "Shell environment for modifying this Nix configuration";
        packages = with pkgs; [
          just
          nixd
          # Deployment tools
          deploy-rs
          #   pkgs.nixos-anywhere
          age
          sops
          ssh-to-age
          git-agecrypt

          # SEC
          vulnix
          sbomnix
        ];
      };
    };
}
