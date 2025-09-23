{
  pkgs,
  lib,
  config,
  ...
}:
{
  config = lib.mkIf config.services.desktopManager.gnome.enable {
    services = {
      udev.packages = [
        pkgs.gnome-settings-daemon
      ];
      desktopManager.gnome = {
        extraGSettingsOverridePackages = [ pkgs.mutter ];

        extraGSettingsOverrides = ''
          	  [org.gnome.mutter]
                    experimental-features=['scale-monitor-framebuffer']

                    [org.gnome.desktop.input-sources]
                    sources=[('xkb', 'us'), ('xkb', 'fi')]

                    [org.freedesktop.ibus.panel.emoji]
                    hotkey="[]"
        '';
      };

      displayManager = {
        gdm = {
          # enable = true;
          wayland = true;
        };

      };
      gnome = {
        games.enable = false;
      };

    };

    environment.gnome.excludePackages = [
      pkgs.epiphany
      pkgs.geary
      pkgs.gnome-maps
      pkgs.gnome-music
      pkgs.gnome-weather
      pkgs.rhythmbox
      pkgs.totem
      pkgs.gnome-tour
    ];
    environment = {
      systemPackages = [
        pkgs.gnome-tweaks
        pkgs.gnomeExtensions.solaar-extension
        pkgs.gnomeExtensions.appindicator
      ];
    };
  };
}
