{ inputs, ... }:
{
  imports = [
    inputs.flake-parts.flakeModules.flakeModules
  ];
  flake = {
    flakeModules = {
      default = ./.;
      omarchy = ./omarchy.nix;
    };
  };
}
