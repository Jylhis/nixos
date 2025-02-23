# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
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
    self.nixosModules.mac-mini-2018

    # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];

  # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in GUI!
  # If no user is logged in, the machine will power down after 20 minutes.
  systemd = {
    services = {
      # FIXME: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
      "getty@tty1".enable = false;
      "autovt@tty1".enable = false;
    };
    coredump.enable = false; # Disable coredumps
    targets = {
      sleep.enable = false;
      suspend.enable = false;
      hibernate.enable = false;
      hybrid-sleep.enable = false;
    };
  };

  # Bootloader.
  boot = {

    #  options amdgpu pcie_gen_cap=0x40000 # TODO(amdgpu)
    extraModprobeConfig = ''
      options i915 mitigations=off
    '';
    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot = {
        enable = true;
        configurationLimit = 3;
      };
    };
    plymouth = {
      enable = true;
      theme = "breeze";
    };

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      "kernel.core_pattern" = "|/bin/false"; # Disable core dumps
      "vm.swappiness" = 1;
      # https://wiki.archlinux.org/title/Sysctl#Virtual_memory
      "vm.dirty_background_bytes" = 4194304;
      "vm.dirty_bytes" = 4194304;
      #"vm.vfs_cache_pressure" = 50;
    };
    initrd = {
      systemd.enable = true;
      #kernelModules = [ "i915" ];
      #availableKernelModules = [ "amdgpu" ];

      # Enable "Silent Boot"
      verbose = false;
    };
    consoleLogLevel = 0;
    kernelParams = [
      "mitigations=off"
      "quiet"
      "boot.shell_on_fail"
      "loglevel=3"
      "rd.systemd.show_status=false"
      "rd.udev.log_level=3"
      "udev.log_priority=3"
      "kvm.enable_virt_at_load=0" # For virtualBox: https://github.com/NixOS/nixpkgs/issues/363887#issuecomment-2536693220
    ];
    loader.timeout = 5;
  };

  networking = {
    hostName = "mac-mini"; # Define your hostname.
    networkmanager.enable = true;
  };

  # Set your time zone.
  time.timeZone = "Europe/Zurich";
  powerManagement.cpuFreqGovernor = "performance";
  # Select internationalisation properties.
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

  services = {
    hardware.bolt.enable = true;
    tailscale.enable = true;
    fstrim.enable = true;

    # Automatically connect any thunderbolt device
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{authorized}=="0", ATTR{authorized}="1"
    '';

    udev.packages = [
      pkgs.gnome-settings-daemon
    ];

    xserver = {
      enable = true;
      videoDrivers = [
        "modesetting"
        "fbdev"
        # "amdgpu" # TODO(amdgpu)
      ];
      displayManager.gdm.enable = true;
      desktopManager.gnome.enable = true;
      # Set layout in GNOME
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

    # Enable CUPS to print documents.
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint ];
    };
    avahi = {
      enable = false;
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      #alsa.support32Bit = false;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;
      audio.enable = true;
      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

  };

  documentation = {
    enable = true;
    doc.enable = true;
    nixos.enable = true;
    man.enable = true;
    dev.enable = true;
    info.enable = true;
  };

  virtualisation = {
    virtualbox.host.enable = true;
    #virtualbox.host.enableExtensionPack = true;
    containers.enable = true;

    docker = {
      enable = true;
      enableOnBoot = true;
      rootless.enable = lib.mkForce false; # Necessary for CDI injection, see https://github.com/NixOS/nixpkgs/issues/337873#issuecomment-2332332343
    };
  };

  users.users.markus.extraGroups = [
    config.users.groups.docker.name
    config.users.groups.vboxusers.name
    config.users.groups.dialout.name # Serial console access. Used for virtualbox
  ];

  environment = {
    gnome.excludePackages = [
      pkgs.epiphany
      pkgs.geary
      pkgs.gnome-maps
      pkgs.gnome-music
      pkgs.gnome-weather
      pkgs.rhythmbox
      pkgs.totem
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
        ffmpeg-full
        unzip
        bash-completion
        gnome-tweaks
        planify
        vlc
        file
        jq
        lsof
        openssl
        pciutils
        tailscale
        vim
      ]
      ++ (with pkgs.gnomeExtensions; [
        solaar-extension
        appindicator
      ]);
  };

  hardware = {
    graphics = {
      enable = true;
      enable32Bit = true;
      # TODO(amdgpu)
      # extraPackages = with pkgs; [
      #   amdvlk
      #   rocmPackages.clr.icd
      # ];
      # extraPackages32 = with pkgs; [
      #   driversi686Linux.amdvlk
      # ];
    };
    logitech.wireless = {
      enable = true;
      enableGraphical = true; # for solaar to be included
    };
    #pulseaudio.enable = lib.mkForce false;
  };

  security.rtkit.enable = true;

  programs = {
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
  # This value determines the NixOS release from which the default
  # settings for stateful data,like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
