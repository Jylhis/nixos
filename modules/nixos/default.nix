# Default NixOS system configuration module.
#
# This module provides common system-wide configurations that apply
# to all NixOS systems in the monorepo, including:
# - Base system locale and services (SSH, tailscale, earlyoom)
# - Nix configuration with garbage collection and optimization
# - Binary cache configuration for faster builds
# - System-wide Nix settings and experimental features
#
# For user-specific configuration, see /modules/home/*
#
# Example usage:
# Import this module in your system configuration to get sensible
# defaults for all NixOS systems.
{
  config,
  lib,
  ...
}:
{

  # A module that automatically imports everything else in the parent folder.
  # imports =
  #   with builtins;
  #   map (fn: ./${fn}) (filter (fn: fn != "default.nix") (attrNames (readDir ./.)));

  config = {

    services = {
      openssh.enable = true;
      earlyoom.enable = true;
      tailscale.enable = true;
    };

    # TODO: organize stuff to sub folders
    boot.loader.systemd-boot.configurationLimit = lib.mkIf config.boot.loader.systemd-boot.enable (
      lib.mkDefault 5
    );

    nix = {
      gc = {
        automatic = lib.mkDefault true;
        options = lib.mkDefault "--delete-older-than 14d";
        randomizedDelaySec = lib.mkDefault "45m";
      };

      optimise.automatic = true;

      settings = {
        download-attempts = lib.mkDefault 2;
        connect-timeout = lib.mkDefault 5;
        fallback = lib.mkDefault true;
        tarball-ttl = lib.mkDefault 604800;
        keep-outputs = lib.mkDefault true;
        download-buffer-size = "256M";
        experimental-features = [
          "nix-command"
          "flakes"
          "ca-derivations"
        ];

        substituters = [
          "https://nix-community.cachix.org"
          "https://jylhis-nixos.cachix.org"
        ];
        trusted-substituters = [
          "https://nix-community.cachix.org"
          "https://jylhis-nixos.cachix.org"
        ];
        trusted-public-keys = [
          "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
          "jylhis-nixos.cachix.org-1:Sk7hqPdA7V0TJvwQakraPOtdPHd4vMrkunpUxub831E="
        ];
      };
    };

    nixpkgs.config = {
      allowUnfree = true;
    };
  };
}
