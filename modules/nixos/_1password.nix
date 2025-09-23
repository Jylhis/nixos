{
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
      };
      _1password-gui = {
        enable = true;
        polkitPolicyOwners = lib.mkDefault config.myusers;
      };
    };
  };
}
