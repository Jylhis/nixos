# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).
{
  self,
  lib,
  unstable,
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

    kernel.sysctl = {
      "kernel.sysrq" = 1;
    };

    kernelParams = [
      "mitigations=off"
    ];

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

  nix.buildMachines = [
    {
      hostName = "lab";
      system = "x86_64-linux";
      sshUser = "nixremote";
      protocol = "ssh-ng";
      publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUcxRmxmRW9lck1Ma1kvYTEvY0l4NTdkbGc2Z2JlcXBzeGJ6SEI4VjlYNksgcm9vdEBtYWNib29rLWFpcgo=";
      sshKey = "/etc/ssh/ssh_host_ed25519_key";
      maxJobs = 3;
      speedFactor = 4;
      supportedFeatures = [
        "nixos-test"
        "benchmark"
        "big-parallel"
        "kvm"
      ];
      mandatoryFeatures = [ ];
    }
  ];
  nix.distributedBuilds = true;
  nix.settings = {
    builders-use-substitutes = true;
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
        sops
        age
        ssh-to-age
        git-agecrypt

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
    ssh.extraConfig = ''
      	    Match Host lab User nix-ssh
                IdentitiesOnly yes
                IdentityFile /etc/ssh/ssh_host_ed25519_key
    '';
    _1password = {
      enable = true;
      package = unstable._1password-cli;

    };
    _1password-gui = {
      enable = true;
      package = unstable._1password-gui;
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

  nixpkgs.config = {
    packageOverrides = pkgs: {
      ffmpeg-full = pkgs.ffmpeg-full.override {
        withUnfree = true;
        withOpengl = true;
      };

    };
  };

  system.stateVersion = "24.11"; # Did you read the comment?

}
