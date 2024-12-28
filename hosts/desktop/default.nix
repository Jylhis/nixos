# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{

  lib,
  pkgs,
  _1password-shell-plugins,
  emacs-overlay,
  ...
}:
{
  imports = [
    ./personal-devenv.nix
    ./pentest-reverse-engineer.nix
    ./cachix.nix
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    _1password-shell-plugins.nixosModules.default
  ];

  nix = {

    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
      randomizedDelaySec = "14m";
    };
    optimise.automatic = true;

    settings = {
      auto-optimise-store = true;
      keep-outputs = true;
      experimental-features = [
        "nix-command"
        "flakes"
        "repl-flake"
      ];
      trusted-users = [
        "root"
        "markus"
      ];

      accept-flake-config = true;
      extra-substituters = [
        "https://cache.soopy.moe"
      ];
      extra-trusted-public-keys = [
        "cache.soopy.moe-1:0RZVsQeR+GOh0VQI9rvnHz55nVXkFardDqfm4+afjPo="
      ];
      extra-trusted-substituters = [
        "https://cache.soopy.moe"
      ];
    };
  };

  # Disable the GNOME3/GDM auto-suspend feature that cannot be disabled in GUI!
  # If no user is logged in, the machine will power down after 20 minutes.
  systemd = {
    services.NetworkManager-wait-online.enable = false;
    targets.sleep.enable = false;
    targets.suspend.enable = false;
    targets.hibernate.enable = false;
    targets.hybrid-sleep.enable = false;

  };

  # Bootloader.
  boot = {
    # binfmt.emulatedSystems =["aarch64-linux"];
    loader.systemd-boot.enable = true;
    loader.systemd-boot.configurationLimit = 5;
    loader.efi.canTouchEfiVariables = true;
    plymouth.enable = true;
    plymouth.theme = "breeze";
    extraModprobeConfig = ''
      options snd-hda-intel model=intel-mac-auto
    '';
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
    ];
    loader.timeout = 5;
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
    networkmanager.enable = true;
  };

  # Set your time zone.
  time.timeZone = "Europe/Zurich";
  powerManagement.cpuFreqGovernor = "performance";
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console.useXkbConfig = true;
  services = {
    fstrim.enable = true;
    tailscale.enable = true;
    hardware.bolt.enable = true;
    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{authorized}=="0", ATTR{authorized}="1"
    '';
    udev.packages = [
      pkgs.gnome.gnome-settings-daemon
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
          xkb-options=['ctrl:swapcaps','terminate:ctrl_alt_bksp', 'lv3:ralt_switch']

          [org.gnome.desktop.interface]
          gtk-theme='org.gnome.desktop.interface'
          color-scheme='prefer-dark'

          [org.freedesktop.ibus.panel.emoji]
          hotkey="[]"
        '';
      };
      videoDrivers = [ "intel" ];
      # Configure keymap in X11
      xkb = {
        layout = "us,fi";
        variant = "";
        options = "ctrl:swapcaps";
      };
    };

    # Enable CUPS to print documents.
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint ];
    };
    avahi = {
      enable = true;
      nssmdns4 = true;
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
  virtualisation.docker.enable = true;
  virtualisation.docker.rootless = {
    enable = true;
    setSocketVariable = true;
  };

  environment = {
    interactiveShellInit = ''
      alias ec='emacsclient -t'
      alias eg='emacsclient -c -a emacs'
      alias eb='emacs -nw -Q'
      alias ebg='emacs -Q'
      alias open='xdg-open'
    '';
    # etc."modprobe.d/amd-egpu-pcie-speed.conf".text = ''
    #   options amdgpu pcie_gen_cap=0x40000
    # '';
    gnome.excludePackages =
      (with pkgs; [
        rhythmbox
        epiphany
      ])
      ++ (with pkgs; [
        geary
        gnome-weather
        gnome-maps
        gnome-music
      ]);

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    # NOTE: Install packages system wide
    systemPackages =
      with pkgs;
      [
        sniffnet
        unzip
        bash-completion
        btop
        cloud-utils
        curlie
        delta
        devenv
        direnv
        docker
        duf
        dust
        eza
        fd
        gimp
        git
        git-lfs
        glances
        gnome.gnome-tweaks
        gnumake
        htop
        iotop
        jq
        k9s
        kubectl
        lsof
        nix-direnv
        nix-ld
        nvidia-docker
        openssl
        pandoc
        pciutils
        planify
        python3Packages.mlflow
        sd
        slack
        solaar
        sshpass
        starship
        tailscale
        vim
        vlc
        yq
        zoxide
      ]
      ++ (with pkgs.gnomeExtensions; [
        solaar-extension
        appindicator
      ]);
  };
  hardware.opengl = {
    enable = true;
    driSupport32Bit = true;
  };

  hardware.logitech.wireless.enable = true;
  hardware.logitech.wireless.enableGraphical = true; # for solaar to be included
  hardware.pulseaudio.enable = lib.mkForce false;
  security.rtkit.enable = true;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.markus = {
    isNormalUser = true;
    description = "Markus";
    extraGroups = [
      "networkmanager"
      "wheel"
      "docker"
    ];
    packages = with pkgs; [
      spotify
      signal-desktop
      nixd
      gopls
      godef
      delve
      source-code-pro
      asm-lsp
      ansible-language-server
    ];
  };

  users.users.sara = {
    isNormalUser = true;
    extraGroups = [
      "networkmanager"
      "wheel"
      "docker"
    ];
    packages = with pkgs; [
      spotify
      signal-desktop
      microsoft-edge
      vscode
      affine

    ];
  };

  programs = {
    #nh.enable = true;
    _1password.enable = true;
    _1password-gui = {
      enable = true;
      polkitPolicyOwners = [ "markus" ];
    };
    _1password-shell-plugins = {
      # enable 1Password shell plugins for bash, zsh, and fish shell
      enable = true;
      # the specified packages as well as 1Password CLI will be
      # automatically installed and configured to use shell plugins
      # plugins = with pkgs; [
      #   glab
      #   cachix
      #   nodePackages.vercel
      # ];
    };
    # Install firefox.
    firefox.enable = true;
    chromium.enable = true;
    java.enable = true;
    direnv.enable = true;
    ccache.enable = true;
    git.lfs.enable = true;
    appimage.enable = true;
    starship.enable = true;
  };

  # Allow unfree packages
  nixpkgs.config = {
    allowUnfree = true;
    warnUndeclaredOptions = true;
  };
  nixpkgs.overlays = [ emacs-overlay.overlay ];
  # This value determines the NixOS release from which the default
  # settings for stateful data,like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
