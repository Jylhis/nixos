{
  lib,
  config,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.boot.plymouth.enable {
    boot = {
      plymouth.themePackages = lib.mkDefault (
        [
          pkgs.nixos-bgrt-plymouth
        ]
        ++ (lib.optionals config.services.desktopManager.plasma6.enable [
          pkgs.kdePackages.breeze-plymouth
        ])
      );
      plymouth.theme = lib.mkIf config.services.desktopManager.plasma6.enable (lib.mkDefault "breeze");

      # Enable "Silent Boot"
      initrd.verbose = false;
      loader.timeout = 5;
      consoleLogLevel = 0;
      kernelParams = [
        "quiet"
        "splash"
        "loglevel=3"
        "rd.systemd.show_status=false"
        "rd.udev.log_level=3"
        "boot.shell_on_fail"
        "udev.log_priority=3"
      ];
    };
  };
}
