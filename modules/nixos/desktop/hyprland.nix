{
  lib,
  config,
  pkgs,
  ...
}:
let
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

  # Enable Hyprland (disabled by default - can be enabled per-machine)
  programs.hyprland = {
    enable = lib.mkDefault true;
    withUWSM = true;
    xwayland.enable = true;
  };

  # XDG Desktop Portal - only add Hyprland portal if Hyprland is enabled
  xdg.portal = lib.mkIf config.programs.hyprland.enable {
    extraPortals = with pkgs; [
      xdg-desktop-portal-hyprland
    ];
  };

  # System packages for Hyprland - only install when Hyprland is enabled
  environment.systemPackages = lib.mkIf config.programs.hyprland.enable (
    with pkgs;
    [
      # Core Hyprland tools
      hyprpicker
      hyprcursor
      hyprpaper

      # Application launcher and bar
      rofi-wayland
      # waybar

      # Screenshot and screen sharing
      grim
      slurp
      wf-recorder

      # Clipboard manager
      wl-clipboard

      # Notification daemon
      # mako

      # Terminal (keeping current preference)
      # kitty

      # Audio control
      pavucontrol

      # Network management GUI
      networkmanagerapplet

      # Blue light filter
      # gammastep

      # Wallpaper setter
      # swww

      # Authentication agent
      kdePackages.polkit-kde-agent-1

      # Keep KDE packages that are useful in any DE
      # filelight
      # partitionmanager
      # ksystemlog
      # isoimagewriter
      # skanlite
      # skanpage
      # kcachegrind
      # arianna
      # francis
      # kalk
      # kcalc
      # kalgebra
    ]
  );

  # Home Manager integration - only enable Hyprland home config when system Hyprland is enabled
  home-manager = lib.mkIf (config.home-manager.enable && config.programs.hyprland.enable) {
    users = mapListToAttrs config.myusers (_name: {
      wayland.windowManager.hyprland.enable = true;
    });
  };
}
