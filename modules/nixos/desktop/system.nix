{ pkgs, lib, ... }:
{
  security.rtkit.enable = true;
  services.pulseaudio.enable = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    pulse.enable = true;
    jack.enable = true;
  };
  # Log in
  services.greetd = {
    enable = true;
    settings.default_session.command =
      "${lib.getExe pkgs.tuigreet} "
      + (lib.cli.toGNUCommandLineShell { } {
        time = true;
        cmd = "Hyprland";
        remember = true;
        remember-session = true;
        user-menu = true;
      });
  };
  services.resolved.enable = true;
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  networking = {
    networkmanager.enable = true;
  };
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-emoji
    nerd-fonts.caskaydia-mono
  ];
  # GPU optimizations
  hardware.graphics = {
    enable = true;
    enable32Bit = true;

    # Intel specific
    extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
      vaapiVdpau
      intel-compute-runtime
    ];
  };
}
