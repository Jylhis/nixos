# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  self,
  lib,
  pkgs,
  config,
  ...
}:
{
  imports = [

    # Include the results of the hardware scan.
    ./hardware-configuration.nix

  ];

  # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in GUI!
  # If no user is logged in, the machine will power down after 20 minutes.
  systemd = {

    targets = {
      sleep.enable = false;
      suspend.enable = false;
      hibernate.enable = false;
      hybrid-sleep.enable = false;
    };
  };

  # Bootloader.
  boot = {
    loader = {
      systemd-boot.enable = true;
      systemd-boot.configurationLimit = 5;
      efi.canTouchEfiVariables = true;
    };
    plymouth = {
      enable = true;
      theme = "breeze";
    };
    #extraModprobeConfig = ''
    #  options snd-hda-intel model=intel-mac-auto
    #'';
    kernel.sysctl = {
      "vm.swappiness" = 1;
      # https://wiki.archlinux.org/title/Sysctl#Virtual_memory
      "vm.dirty_background_bytes" = 4194304;
      "vm.dirty_bytes" = 4194304;
    };
    initrd.systemd.enable = true;

    # Enable "Silent Boot"
    initrd.verbose = false;
    consoleLogLevel = 0;
    kernelParams = [
      "quiet"
      "splash"
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

  apple-hardware.enableFirmware = false;
  apple-hardware.firmware = self.outputs.packages.x86_64-linux.brcm-firmware.override {
    name = "firmware-mac-mini.tar";
    hash = "sha256-nQmzaCAIcApl0ihSz/dV2z8iYPTGKBo04+Wxr3Uh7hc=";
  };

  services = {
    hardware.bolt.enable = true;
    tailscale.enable = true;
    fstrim.enable = true;
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
      nssmdns4 = false;
      openFirewall = true;
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
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
    nixos.enable = true;
    man.enable = true;
    dev.enable = true;
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
  ];

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

        # General
        unzip
        bash-completion

        # Development
        cloud-utils
        curlie
        delta
        devenv
        direnv
        duf
        dust
        eza
        fd
        git-lfs
        git
        gnumake
        clang-tools
        gcc14
        gdb
        rr
        pkg-config
        rtags
        cppcheck
        valgrind
        python3
        nodejs_22
        jdk
        git

        # Monitoring
        sniffnet
        btop
        glances
        htop
        iotop

        # Applications
        gimp
        gnome-tweaks
        pandoc
        planify
        vlc

        # Kubernetes
        k9s
        kubectl

        # Nix
        nix-direnv
        nix-ld
        nix-output-monitor

        # Other
        file
        man
        man-pages
        linux-manual
        man-pages-posix
        clang-manpages
        stdmanpages
        jq
        lsof
        openssl

        pciutils

        sd

        solaar
        sshpass
        starship
        tailscale
        vim
        yq
        zoxide
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
    };
    logitech.wireless = {
      enable = true;
      enableGraphical = true; # for solaar to be included
    };
    #pulseaudio.enable = lib.mkForce false;
  };

  security.rtkit.enable = true;

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

  };

  # This value determines the NixOS release from which the default
  # settings for stateful data,like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
