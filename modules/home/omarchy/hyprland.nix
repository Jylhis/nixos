# TODO:
# - Darkmode/lightmode
# - Autostart: 1password
{
  lib,
  config,
  ...
}:
{
  config = lib.mkIf config.wayland.windowManager.hyprland.enable {

    qt = {
      style = {
        name = if config.colorScheme.variant == "light" then "adwaita" else "adwaita-dark";

      };
    };

    gtk = {
      theme = {
        name = if config.colorScheme.variant == "light" then "Adwaita" else "Adwaita:dark";
      };
    };

    wayland.windowManager.hyprland = {
      settings = {
        # Default apps
        "$terminal" = "kitty";
        "$fileManager" = "nautilus --new-window";
        "$browser" = "brave --new-window --ozone-platform=wayland";
        "$music" = "spotify";
        "$passwordManager" = "1password";
        "$notes" = "obsidian";
        "$messenger" = "signal-desktop";
        "$webapp" = "$browser --app";

        monitor = [
          "desc:Samsung Electric Company Odyssey G5 HK2WC01719, 2560x1440@144, 0x0, 1"
          # Primary 4K monitor
          # "DP-1, 3840x2160@60, 0x0, 2.0"
          # Laptop display
          # "eDP-1, 2560x1600@165, 1920x0, 1.5"
          # Fallback for any monitor
        ];

        # Enhanced input configuration
        input = {
          kb_layout = "us,fi";
          kb_options = "ctrl:swapcaps,grp:win_space_toggle";
        };
      };
    };
  };
}
