{ lib, config, ... }:
{
  config = lib.mkIf config.boot.plymouth.enable {
    boot = {
      plymouth.theme = lib.mkDefault "breeze";
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
