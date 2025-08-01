# User configuration module
{ config, lib, ... }:
{
  options = {
    me = {
      username = lib.mkOption {
        type = lib.types.str;
        description = "Your username as shown by `id -un`";
      };
      fullname = lib.mkOption {
        type = lib.types.str;
        description = "Your full name for use in Git config";
      };
      email = lib.mkOption {
        type = lib.types.str;
        description = "Your email for use in Git config";
      };
      publicKeys = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };

      # TODO: Move to different namespace?
      flakePath = lib.mkOption {
        type = lib.types.str;
        description = "Path to the flake config in the system";
        default = "${config.home.homeDirectory}/nixos-config";
      };
    };
  };
  config = {
    home.username = config.me.username;
  };
}
