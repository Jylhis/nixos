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

  programs.hyprland = {
    enable = lib.mkDefault true;
    withUWSM = true;
    xwayland.enable = true;
  };

  xdg.portal = lib.mkIf config.programs.hyprland.enable {
    # enable = true;
    # wlr.enable = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-hyprland
      # xdg-desktop-portal-gtk
    ];
    # configPackages = [ pkgs.xdg-desktop-portal-hyprland ];
  };

  # Audio with PipeWire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    wireplumber.enable = true;
  };

  # Network management
  networking.networkmanager.enable = true;

  # Bluetooth
  hardware.bluetooth = {
    enable = true;
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

  # GPU optimizations
  hardware.graphics = {
    enable = true;
    enable32Bit = true;

    # Intel specific
    extraPackages =
      with pkgs;
      lib.optionals config.hardware.graphics.enable [
        intel-media-driver
        intel-vaapi-driver
        vaapiVdpau
        intel-compute-runtime
      ];
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
  environment.systemPackages = lib.mkIf config.programs.hyprland.enable (
    with pkgs;
    [
      # Core Hyprland tools
      hyprpicker
      hyprcursor
      hyprpaper

      # Modern panel and notifications
      hyprpanel
      swaynotificationcenter

      # Launcher alternatives
      fuzzel
      rofi-wayland

      # Wallpaper and theming
      swww
      wallust
      pywal

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
      blueman

      # Performance tools
      btop

      # File management
      xdg-utils
      mimeo

      # Authentication
      kdePackages.polkit-kde-agent-1

      # Fonts
      nerd-fonts.jetbrains-mono
      nerd-fonts.fira-code
      nerd-fonts.iosevka
    ]
  );

  # Security
  security = {
    polkit.enable = true;
    pam.services.hyprlock = { };
  };

  # Home Manager integration - only enable Hyprland home config when system Hyprland is enabled
  home-manager = lib.mkIf (config.home-manager.enable && config.programs.hyprland.enable) {
    users = mapListToAttrs config.myusers (_name: {
      wayland.windowManager.hyprland.enable = true;
    });
  };
}
