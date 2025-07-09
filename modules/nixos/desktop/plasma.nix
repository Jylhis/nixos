{ pkgs, ... }:
{
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
}
