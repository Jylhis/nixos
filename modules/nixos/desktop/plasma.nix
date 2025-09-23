{
  lib,
  config,
  pkgs,
  ...
}:
{

  imports = [ ../users.nix ];
  config = lib.mkIf config.services.desktopManager.plasma6.enable {
    qt.enable = true;
    services = {
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

    # FIXME: homeModule plasma is dependent on this
  };
}
