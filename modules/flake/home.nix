{
  self,
  lib,
  inputs,
  ...
}:
let
  inherit
    (import ../../lib/flakes.nix {
      inherit (self) inputs;
      inherit self lib;
    })
    forAllNixFiles
    mkHomeConfiguration
    ;
in
{
  imports = [
    inputs.home-manager.flakeModules.home-manager
  ];

  flake = {
    homeModules = forAllNixFiles "${self}/modules/home" (fn: fn) // {
      common =
        { config, pkgs, ... }:
        {
          # Cross-platform home directory detection
          # Linux: /home/username, macOS: /Users/username
          home.homeDirectory = lib.mkDefault "/${
            if pkgs.stdenv.isDarwin then "Users" else "home"
          }/${config.home.username}";

          # macOS-specific PATH additions for Nix integration
          home.sessionPath = lib.mkIf pkgs.stdenv.isDarwin [
            "/etc/profiles/per-user/$USER/bin" # To access home-manager binaries
            "/nix/var/nix/profiles/system/sw/bin" # To access nix-darwin binaries
            "/usr/local/bin" # Some macOS GUI programs install here
          ];
        };
    };
  };

  perSystem =
    { pkgs, ... }:
    {
      legacyPackages.homeConfigurations = forAllNixFiles "${self}/configurations/home" (
        fn: mkHomeConfiguration pkgs fn
      );
    };
}
