{
  lib,
  config,
  pkgs,
  flake,
  ...
}:
let
  inherit (flake) inputs;
  mapListToAttrs =
    m: f:
    lib.listToAttrs (
      map (name: {
        inherit name;
        value = f name;
      }) m
    );
in
{

  imports = [ ../users.nix ];
  qt.enable = true;
  services = {
    desktopManager.plasma6 = {
      enable = true;
    };
    displayManager = {
      sddm = {
        enable = true;
        wayland.enable = true;
        settings.General.DisplayServer = "wayland";
      };
    };
  };
  environment.systemPackages = with pkgs.kdePackages; [
    filelight
    partitionmanager
    ksystemlog
    isoimagewriter
    skanlite
    skanpage
    kcachegrind
    falkon
    arianna
    francis
    calligra
    kalk
    kcalc
    kalgebra
  ];

  home-manager = lib.mkIf config.home-manager.enable {
    users = mapListToAttrs config.myusers (_name: {
      imports = [ inputs.plasma-manager.homeManagerModules.plasma-manager ];
    });
  };
}
