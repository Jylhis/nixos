{ pkgs, config, ... }:
{
  programs = {
    _1password = {
      enable = true;
      package = pkgs._1password-cli;
    };
    _1password-gui = {
      enable = true;
      package = pkgs._1password-gui;
      polkitPolicyOwners = config.myusers;
    };
  };
}
