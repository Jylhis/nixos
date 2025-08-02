# This is your nixos configuration.
# For home configuration, see /modules/home/*
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

  # Set base system locale to English
  i18n.defaultLocale = lib.mkDefault "en_GB.UTF-8";

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
}
