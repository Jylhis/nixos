{ config, lib, ... }:
{
  # https://nixos.asia/en/direnv
  programs.direnv = lib.mkIf config.programs.direnv.enable {
    silent = true;
    nix-direnv = {
      enable = true;
    };
    config.global = {
      # Make direnv messages less verbose
      hide_env_diff = true;
    };
  };
}
