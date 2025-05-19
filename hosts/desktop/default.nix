{
  self,
  unstable,
  pkgs,
  config,
  ...
}:
{
  imports = [
    self.nixosModules.hardware-mac-mini-2018
    ./hardware-configuration.nix
    ../../modules/roles/desktop
    ../../modules/roles/nix-companion-server.nix
  ];

  jylhis.role.desktop.enable = true;

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

  # NetworkManager-wait-online.service fails to start
  # https://github.com/NixOS/nixpkgs/issues/180175
  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.network.wait-online.enable = false;
  boot.initrd.systemd.network.wait-online.enable = false;

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
      grub.configurationLimit = 2;
    };
    plymouth.enable = true;

    kernel.sysctl = {
      "kernel.sysrq" = 1;
      "kernel.core_pattern" = "|/bin/false"; # Disable core dumps
      #"vm.swappiness" = 1;
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
    earlyoom.enable = true;
    tailscale.enable = true;
    hardware.bolt.enable = true;
    fstrim.enable = true;

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
    pipewire = {
      enable = true;
      alsa.enable = true;
      pulse.enable = true;
      audio.enable = true;
    };
  };

  documentation.enable = true;

  virtualisation = {
    virtualbox.host.enable = true;
    containers.enable = true;

    podman.enable = true;
  };

  users.users.markus.extraGroups = [
    config.users.groups.podman.name
    config.users.groups.vboxusers.name
    config.users.groups.dialout.name # Serial console access. Used for virtualbox
  ];

  environment = {
    #sessionVariables.VK_ICD_FILENAMES = "/run/opengl-driver/share/vulkan/icd.d/nvidia_icd.x86_64.json";

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    # NOTE: Install packages system wide
    systemPackages =
      with pkgs;
      [
        docker-compose
        # Graphics debug stuff
        vulkan-tools
        libva-utils
        glxinfo
        clinfo

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
    bluetooth.enable = true;
    flipperzero.enable = true;
    logitech.wireless = {
      enable = true;
      enableGraphical = true; # for solaar to be included
    };
  };

  security.rtkit.enable = true;

  programs = {
    nix-ld = {
      enable = true;
    };
    # ssh.extraConfig = ''
    #   	    Match Host lab User nix-ssh
    #             IdentitiesOnly yes
    #             IdentityFile /etc/ssh/ssh_host_ed25519_key
    # '';
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
      enable = false;
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
