{
  pkgs,
  config,
  lib,
  ...
}:
{
  imports = [
    ./users.nix
  ];
  config = {

    programs = {
      _1password = {
        enable = true;
        package = lib.mkDefault pkgs._1password-cli;
      };
      _1password-gui = {
        enable = true;
        package = lib.mkDefault pkgs._1password-gui;
        polkitPolicyOwners = lib.mkDefault config.myusers;
      };
    };
  };
}
