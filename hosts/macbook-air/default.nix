{
  self,
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
      };
    };
    plymouth.enable = true;

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

  console.useXkbConfig = true;

  # NetworkManager-wait-online.service fails to start
  # https://github.com/NixOS/nixpkgs/issues/180175
  systemd.services.NetworkManager-wait-online.enable = false;
  systemd.network.wait-online.enable = false;
  boot.initrd.systemd.network.wait-online.enable = false;
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

  environment = {
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
    };

    vim = {
      enable = true;
    };

  };

  system.stateVersion = "24.11"; # Did you read the comment?
}
