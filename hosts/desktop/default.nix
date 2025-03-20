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
    self.nixosModules.mac-mini-2018
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
        configurationLimit = 2;
      };
    };
    plymouth = {
      enable = true;
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

  powerManagement.cpuFreqGovernor = "performance";

  console.useXkbConfig = true;

  services = {
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
      displayManager.gdm = {
        enable = true;
        wayland = true;
      };

      desktopManager.gnome = {
        enable = true;
        extraGSettingsOverridePackages = [ pkgs.mutter ];

        extraGSettingsOverrides = ''
          	  [org.gnome.mutter]
                    experimental-features=['scale-monitor-framebuffer']

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
      brave = pkgs.brave.override {
        commandLineArgs = "--enable-features=VaapiVideoDecodeLinuxGL,VaapiVideoEncoder,Vulkan,VulkanFromANGLE,DefaultANGLEVulkan,VaapiIgnoreDriverChecks,VaapiVideoDecoder,PlatformHEVCDecoderSupport,UseMultiPlaneFormatForHardwareVideo,UseOzonePlatform --ozone-platform=wayland";
      };

    };
  };
  system.stateVersion = "24.05"; # Did you read the comment?
}
