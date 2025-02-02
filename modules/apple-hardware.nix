{

  lib,
  nixos-hardware,
  config,
  ...
}:
with lib;
let
  cfg = config.apple-hardware;
in
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
    nixos-hardware.nixosModules.apple-t2
  ];

  options.apple-hardware = {

    enableFirmware = mkOption {
      type = lib.types.bool;

      default = false;
    };

    firmware = mkOption {
      type = lib.types.package;
      description = "Firmware package https://wiki.t2linux.org/distributions/nixos/installation/#imperative-setup";
      default = null;
    };
  };

  config = {
    # Option definitions.
    # Define what other settings, services and resources should be active.
    # Usually these depend on whether a user of this module chose to "enable" it
    # using the "option" above.
    # Options for modules imported in "imports" can be set here.
    nix.settings = {
      extra-substituters = [
        #"https://hydra.soopy.moe"
        "https://cache.soopy.moe"
      ];
      extra-trusted-public-keys = [
        "hydra.soopy.moe:IZ/bZ1XO3IfGtq66g+C85fxU/61tgXLaJ2MlcGGXU8Q="
        "cache.soopy.moe-1:0RZVsQeR+GOh0VQI9rvnHz55nVXkFardDqfm4+afjPo="
      ];
      extra-trusted-substituters = [
        "https://cache.soopy.moe"
        #"https://hydra.soopy.moe"
      ];
    };

    services.fwupd.enable = false;

    hardware.firmware = mkIf cfg.enableFirmware [ cfg.firmware ];

  };
}
