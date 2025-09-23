{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [ ../users.nix ];

  config = lib.mkIf config.programs.hyprland.enable {
    xdg.portal = {
      enable = true;
      wlr.enable = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-hyprland
        xdg-desktop-portal-gtk
        xdg-desktop-portal-gnome
        xdg-desktop-portal
      ];
    };

    # Bluetooth
    hardware.bluetooth = lib.mkIf config.hardware.bluetooth.enable {
      powerOnBoot = true;
      settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
          Experimental = true;
        };
      };
    };

    # Power management
    services = {
      power-profiles-daemon.enable = true;
      upower.enable = true;
      thermald.enable = true;
    };

    # Environment variables for Wayland
    environment.sessionVariables = {
      NIXOS_OZONE_WL = "1";
      MOZ_ENABLE_WAYLAND = "1";
      ELECTRON_OZONE_PLATFORM_HINT = "wayland";
      XDG_CURRENT_DESKTOP = "Hyprland";
      XDG_SESSION_DESKTOP = "Hyprland";
      XDG_SESSION_TYPE = "wayland";
    };

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

      # Fonts
      nerd-fonts.jetbrains-mono
      nerd-fonts.fira-code
      nerd-fonts.iosevka
    ];
    programs.dconf.enable = true;

    # Security
    security = {
      # TODO: polkit, PAM
      polkit.enable = true;
      pam.services.hyprlock = { };
    };
  };
}
