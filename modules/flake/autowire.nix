{
  inputs,
  self,
  lib,
  ...
}:
let
  inherit
    (import ../../lib/flakes.nix {
      inherit (self) inputs;
      inherit self lib;
    })
    forAllNixFiles
    mkLinuxSystem
    mkMacosSystem
    ;
in
{

  flake = {
    modules = {
    };
    darwinConfigurations = forAllNixFiles "${self}/configurations/darwin" (
      fn: mkMacosSystem { home-manager = true; } fn
    );

    nixosConfigurations = forAllNixFiles "${self}/configurations/nixos" (
      fn: mkLinuxSystem { home-manager = true; } fn
    );

    darwinModules = forAllNixFiles "${self}/modules/darwin" (fn: fn);

    nixosModules = forAllNixFiles "${self}/modules/nixos" (fn: fn);

    homeModules = forAllNixFiles "${self}/modules/home" (fn: fn);

    overlays = forAllNixFiles "${self}/overlays" (
      fn:
      import fn
        (import ../../lib/flakes.nix {
          inherit (self) inputs;
          inherit self lib;
        }).specialArgsFor.common
    );

  };
}
