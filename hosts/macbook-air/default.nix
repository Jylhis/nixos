# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  self,
  pkgs,
  config,
  ...
}:

{
  imports = [
    self.nixosModules.macbook-air
    ./hardware-configuration.nix
  ];
  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      systemd-boot.configurationLimit = 5;
      efi.canTouchEfiVariables = true;
    };
    plymouth = {
      enable = true;
      theme = "breeze";
    };

  };

  networking = {
    hostName = "macbook-air"; # Define your hostname.
    networkmanager.enable = true;
  };

  time.timeZone = "Europe/Zurich";
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "de_CH.UTF-8";
      LC_IDENTIFICATION = "de_CH.UTF-8";
      LC_MEASUREMENT = "de_CH.UTF-8";
      LC_MONETARY = "de_CH.UTF-8";
      LC_NAME = "de_CH.UTF-8";
      LC_NUMERIC = "de_CH.UTF-8";
      LC_PAPER = "de_CH.UTF-8";
      LC_TELEPHONE = "de_CH.UTF-8";
      LC_TIME = "de_CH.UTF-8";
    };
  };

  console.useXkbConfig = true;

  # NetworkManager-wait-online.service fails to start
  # https://github.com/NixOS/nixpkgs/issues/180175
  # systemd.services.NetworkManager-wait-online.enable = false;
  # systemd.network.wait-online.enable = false;
  # boot.initrd.systemd.network.wait-online.enable = false;

  services = {

    tailscale.enable = true;
    hardware.bolt.enable = true;

    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{authorized}=="0", ATTR{authorized}="1"
    '';
    udev.packages = [
      pkgs.gnome-settings-daemon
    ];

    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      desktopManager.gnome = {
        extraGSettingsOverrides = ''
          [org.gnome.desktop.input-sources]
          sources=[('xkb', 'us'), ('xkb', 'fi')]

          [org.gnome.desktop.interface]
          gtk-theme='org.gnome.desktop.interface'
          color-scheme='prefer-dark'

          [org.freedesktop.ibus.panel.emoji]
          hotkey="[]"
        '';
      };
      xkb = {
        layout = "us,fi";
        variant = "";
      };
    };
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint ];
    };

  };

  environment = {
    gnome.excludePackages = with pkgs; [
      totem
      rhythmbox
      geary
      gnome-weather
      gnome-maps
      gnome-music
      epiphany
    ];

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    # NOTE: Install packages system wide
    systemPackages =
      with pkgs;
      [

        unzip
        bash-completion
        btop

        curlie
        delta
        devenv
        direnv

        # duf
        #dust
        #eza
        fd

        gnome-tweaks
        gnumake
        htop
        iotop
        jq

        lsof
        nix-direnv
        nix-ld

        openssl
        pandoc
        pciutils
        planify
        #sd

        #sshpass
        starship

        vlc
        yq
        #zoxide
      ]
      ++ (with pkgs.gnomeExtensions; [
        solaar-extension
        appindicator
      ]);
  };

  hardware = {
    logitech.wireless = {
      enable = true;
      enableGraphical = true; # for solaar to be included
    };
  };

  programs = {
    _1password.enable = true;
    _1password-gui = {
      enable = true;
      polkitPolicyOwners = [
        config.users.users.markus.name
        config.users.users.sara.name
      ];
    };

    # Install firefox.
    firefox = {
      enable = true;
      languagePacks = [
        "en-US"
        "en-GB"
        "fi"
        "de"
        "fr"
      ];
    };

    vim = {
      enable = true;
    };

  };

  system.stateVersion = "24.11"; # Did you read the comment?

}
