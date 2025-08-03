{
  lib,
  flake-parts-lib,
  inputs,
  flake,
  config,
  ...
}:

let
  inherit (inputs) deploy-rs;
  deployType =
    { lib, ... }:
    {
      options.nodes = lib.mkOption {
        type = with lib.types; attrsOf anything;
        description = ''
          A set of deploy-rs nodes.
        '';
      };
    };
in
{
  options = {
    flake = flake-parts-lib.mkSubmoduleOptions {
      deploy = lib.mkOption {
        type = with lib.types; submodule deployType;
        default = { };
        description = ''
          An attribute set of deploy-rs nodes
        '';
      };
    };
  };
  config = {
    # flake.deploy.nodes ={};
    flake.deploy.nodes = {
      lab = {
        hostname = "lab";
        sshUser = "root";
        autoRollback = true;
        magicRollback = true;
        profiles.system = {
          user = "root";
          path = deploy-rs.lib.x86_64-linux.activate.nixos config.flake.nixosConfigurations.lab;
        };
      };
    };

    flake.checks = builtins.mapAttrs (
      _system: deployLib: deployLib.deployChecks config.flake.deploy
    ) deploy-rs.lib;

  };

}

#}
