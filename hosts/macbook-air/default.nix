{
  self,
  lib,
  nix-software-center,
  nixos-conf-editor,
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

  # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in GUI!
  # If no user is logged in, the machine will power down after 20 minutes.
  systemd = {
    services = {
      # FIXME: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
      "getty@tty1".enable = false;
      "autovt@tty1".enable = false;
    };
    coredump.enable = false; # Disable coredumps
  };

  # Bootloader.
  boot = {
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
      theme = lib.mkDefault "breeze";
    };

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      "kernel.core_pattern" = "|/bin/false"; # Disable core dumps
    };
    initrd = {
      systemd.enable = true;

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
    ];
    loader.timeout = 5;
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
    earlyoom.enable = true;
    tailscale.enable = true;
    hardware.bolt.enable = true;
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
      desktopManager.gnome = {
        extraGSettingsOverrides = ''
          [org.gnome.desktop.input-sources]
          sources=[('xkb', 'us'), ('xkb', 'fi')]

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
    gnome.excludePackages = [
      pkgs.epiphany
      pkgs.geary
      pkgs.gnome-maps
      pkgs.gnome-music
      pkgs.gnome-weather
      pkgs.rhythmbox
      pkgs.totem
      pkgs.gnome-tour
    ];

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    # NOTE: Install packages system wide
    systemPackages =
      with pkgs;
      [
        nix-software-center.packages.${system}.nix-software-center
        nixos-conf-editor.packages.${system}.nixos-conf-editor
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
    };
    logitech.wireless = {
      enable = true;
      enableGraphical = true; # for solaar to be included
    };
  };

  security.rtkit.enable = true;

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
