{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [ ../users.nix ];

  config = lib.mkIf config.programs.hyprland.enable {

    # Required packages for the complete ecosystem
    environment.systemPackages = with pkgs; [
      # Core Hyprland tools
      hyprpicker
      hyprcursor
      hyprpaper
      hyprshot

      hyprsunset
      brightnessctl
      pamixer

      gnome-themes-extra

      # Monitor management
      kanshi
      wlr-randr

      # Screenshots and recording
      grim
      slurp
      wf-recorder

      # Clipboard
      cliphist
      wl-clipboard
      wl-clip-persist

      # System utilities
      brightnessctl
      playerctl
      pavucontrol
      pwvucontrol
      networkmanagerapplet

      # File management
      xdg-utils
      mimeo
    ];
    programs.dconf.enable = true;
  };
}
