# Add your reusable NixOS modules to this directory, on their own file (https://nixos.wiki/wiki/Module).
# These should be stuff you would like to share with others, not your personal configurations.
{
  rclone-sync = import ./rclone-sync;
  personal-defaults = import ./personal-default.nix;
  nix-companion = import ./nix-companion-server.nix;
  apple-hardware = import ./apple-hardware.nix;
  jyl-cachix = import ./cachix.nix;

  mac-mini-2018 = import ./hardware/mac-mini-2018.nix;
  macbook-air = import ./hardware/macbook-air.nix;
}
