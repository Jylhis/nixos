# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{
  config,
  #  lib,
  pkgs,
  # _1password-shell-plugins,
  emacs-overlay,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.

    ../../pentest-reverse-engineer.nix
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
  hardware.firmware = [
    (pkgs.stdenvNoCC.mkDerivation (_: {
      name = "brcm-firmware";
      src = pkgs.requireFile {
        #  nix store add-file --name macbook-air-firmware.tar.gz
        name = "macbook-air-firmware.tar.gz";
        message = "asdf";
        hash = "sha256-kbBB0HgbUBwDMqTJwLo3ykDs1mmx8dj1JUXbXGLVQss=";
      };
      unpackPhase = ''
        mkdir -p $out/lib/firmware/brcm
        tar xzf $src --dir "$out/lib/firmware/brcm"
      '';
    }))
  ];
  powerManagement.enable = true;

  };

  networking = {
    hostName = "macbook-air"; # Define your hostname.
    networkmanager.enable = true;
  };

  time.timeZone = "Europe/Zurich";
  i18n.defaultLocale = "en_US.UTF-8";

  console.useXkbConfig = true;

  services = {
    thermald.enable = true;
    # tlp = {
    #   enable = true;
    #   settings = {
    #     CPU_SCALING_GOVERNOR_ON_AC = "performance";
    #     CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

    #     CPU_ENERGY_PERF_POLICY_ON_BAT = "power";
    #     CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

    #     CPU_MIN_PERF_ON_AC = 0;
    #     CPU_MAX_PERF_ON_AC = 100;
    #     CPU_MIN_PERF_ON_BAT = 0;
    #     CPU_MAX_PERF_ON_BAT = 20;

    #     #Optional helps save long term battery health
    #     START_CHARGE_THRESH_BAT0 = 40; # 40 and bellow it starts to charge
    #     STOP_CHARGE_THRESH_BAT0 = 80; # 80 and above it stops charging

    #   };
    # };

    # auto-cpufreq.enable = true;
    # auto-cpufreq.settings = {
    #   battery = {
    #     governor = "powersave";
    #     turbo = "never";

    #   };
    #   charger = {
    #     governor = "performance";
    #     turbo = "auto";
    #   };
    # };
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
          xkb-options=['ctrl:swapcaps','terminate:ctrl_alt_bksp', 'lv3:ralt_switch']

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
        options = "ctrl:swapcaps";
      };
    };
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint ];
    };
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
    gnome.excludePackages = with pkgs; [
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
        gnome-tweaks
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

  hardware = {
    logitech.wireless = {
      enable = true;
      enableGraphical = true; # for solaar to be included
    };
  };

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

  system.stateVersion = "24.11"; # Did you read the comment?

}
