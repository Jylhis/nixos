{
  lib,
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./users.nix
  ];
  security.rtkit.enable = true; # TODO: What is this?
  programs.nix-ld.enable = true;
  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    bluetooth.enable = true;
  };
  nix.daemonCPUSchedPolicy = "idle";

  # TODO: Organize

  boot.plymouth.theme = lib.mkIf config.boot.plymouth.enable (lib.mkDefault "breeze");
  boot.kernelParams = [ "mitigations=off" ];
  time.timeZone = lib.mkDefault "Europe/Zurich";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "all"
    ];
    # extraLocaleSettings = {
    #   LC_ADDRESS = "en_US.UTF-8";
    #   LC_IDENTIFICATION = "de_CH.UTF-8";
    #   LC_MEASUREMENT = "de_CH.UTF-8";
    #   LC_MONETARY = "de_CH.UTF-8";
    #   LC_NAME = "de_CH.UTF-8";
    #   LC_NUMERIC = "de_CH.UTF-8";
    #   LC_PAPER = "de_CH.UTF-8";
    #   LC_TELEPHONE = "de_CH.UTF-8";
    #   LC_TIME = "de_CH.UTF-8";
    # };
  };

  documentation = lib.mkIf config.documentation.enable {
    doc.enable = true;
    nixos = {
      enable = true;
      #includeAllModules = true;
    };
    man = {
      enable = true;
    };
    dev.enable = true;
    info.enable = true;
  };
  # Enable HEIC support
  environment.systemPackages = [
    pkgs.libheif
    pkgs.libheif.out
    # attempt to fix following errors when plugging in usb stick:
    # .gvfsd-wsdd-wra[4217]: Failed to spawn the wsdd daemon: Failed to execute child process “wsdd” (No such file or directory)
    # Couldn't create directory monitor on wsdd:///. Error: Automount failed: Failed to spawn the underlying wsdd daemon.
    # Source: https://forums.linuxmint.com/viewtopic.php?p=2526298&sid=1221d1580704693bbaaf3be6de57f10c#p2526298
    pkgs.wsdd
  ];
  environment.pathsToLink = [ "share/thumbnailers" ];

  programs.firefox.languagePacks = lib.optionals config.programs.firefox.enable [
    "en-US"
    "en-GB"
    "fi"
    "de"
    "fr"
  ];
  # REVIEW: Is this needed?
  services.udev.extraRules = lib.mkDefault ''ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{authorized}=="0", ATTR{authorized}="1"'';
  environment.gnome.excludePackages =
    lib.optionals config.services.xserver.desktopManager.gnome.enable
      [
        pkgs.epiphany
        pkgs.geary
        pkgs.gnome-maps
        pkgs.gnome-music
        pkgs.gnome-weather
        pkgs.rhythmbox
        pkgs.totem
        pkgs.gnome-tour
      ];

}
