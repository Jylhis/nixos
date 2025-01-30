# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  self,
  lib,
  pkgs,
  config,
  _1password-shell-plugins,
  emacs-overlay,
  ...
}:
{
  imports = [
    ../../cachix.nix
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
      accept-flake-config = true;
      auto-optimise-store = true;
      keep-outputs = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      trusted-users = [
        config.users.users.root.name
        config.users.users.markus.name
      ];

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
    loader.systemd-boot.enable = true;
    loader.systemd-boot.configurationLimit = 5;
    loader.efi.canTouchEfiVariables = true;
    plymouth.enable = true;
    plymouth.theme = "breeze";
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
    fwupd.enable = true;

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
      #videoDrivers = [ "modesetting" ];
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

  environment = {

    gnome.excludePackages = (
      with pkgs;
      [
        epiphany
        rhythmbox
        geary
        gnome-maps
        gnome-music
        gnome-weather
      ]
    );

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

  users = {

    # Define a user account. Don't forget to set a password with ‘passwd’.
    users = {

      markus = {
        isNormalUser = true;
        description = "Markus";
        extraGroups = [
          config.users.groups.networkmanager.name # For managing network connections
          config.users.groups.wheel.name # For sudo
          config.users.groups.docker.name
          config.users.groups.vboxusers.name
        ];
        packages = with pkgs; [
          # General applications
          spotify
          signal-desktop

          # Dev tools
          nix-diff
          ansible-language-server
          asm-lsp
          moreutils
          delve
          godef
          gopls
          nixd
          source-code-pro

          wget
          planify
          starship
          tailscale
          jetbrains.datagrip
          minio-client

          # CLI utils
          bat
          direnv
          nix-direnv
          git

          # Emacs support packages
          emacs-all-the-icons-fonts
          source-code-pro

          # Devtools (moved from emacs-markus)

          marksman
          ## Common
          ripgrep

          ## python
          python3Packages.python-lsp-server
          ruff

          ## Go
          # Golang
          go
          gopls
          godef
          delve

          # Nix
          statix
          nixd
          nixfmt-rfc-style

          # vala
          vala
          vala-lint

          # SQL
          sqls
          sqlint

          # Haskell
          ghc
          haskell-language-server
          cabal-install

          # C# dotnet
          dotnet-sdk
          csharp-ls

          # HTML + CSS
          stylelint

          # Config languages
          yamllint

          # Assembly
          nasm
          asm-lsp

          # Build tools
          cmake
          gnumake

          # Javascript & Typescript
          eslint
          typescript
          nodePackages.jsdoc

          # Docker
          hadolint
        ];
      };

      sara = {
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
    };
  };

  home-manager.users.markus =
    {
      config,
      nixosConfig,
      pkgs,
      ...
    }:
    {
      services = {
        emacs = {
          enable = true;
          package = self.outputs.packages.x86_64-linux.emacs-markus;
          client.enable = true;
          client.arguments = [
            "-c"
            "-a"
            "emacs"
          ];
          defaultEditor = true;
          socketActivation.enable = true;
        };
      };
      programs = {
        nix-index.enable = true;
        bash.enable = true;
        readline = {
          enable = true;
          variables = {
            colored-completion-prefix = true; # Enable coloured highlighting of completions
            completion-ignore-case = true; # Auto-complete files with the wrong case
            revert-all-at-newline = true; # Don't save edited commands until run
            show-all-if-ambiguous = true;
          };
        };
        git = {
          enable = true;
          userEmail = lib.mkForce "markus@jylhis.com";
          userName = "Jylhis";
          ignores = [
            "*~"
            "\#*\#"
            "*.elc"
            ".\#*"
            "[._]*.sw[a-p]"
          ];

        };
        emacs = {
          enable = true;
          package = self.outputs.packages.x86_64-linux.emacs-markus;
        };
        direnv.enable = true;
        starship.enable = true;
        ssh.enable = true;
        ssh.extraConfig = ''
          IdentityAgent ~/.1password/agent.sock
        '';

      };

      home = {
        keyboard = {
          options = [
            "ctrl:swapcaps"
            "terminate:ctrl_alt_bksp"
            "lv3:ralt_switch"
          ];
        };
        shellAliases = {
          ec = "emacsclient -t";
          eg = "emacsclient -c -a emacs";
          eb = "emacs -nw -Q";
          ebg = "emacs -Q";
          open = "xdg-open";
        };

        stateVersion = "24.11";
      };
    };

  programs = {
    command-not-found.enable = false;
    nix-index.enable = true;
    _1password.enable = true;
    _1password-gui = {
      enable = true;
      polkitPolicyOwners = [ config.users.users.markus.name ];
    };
    _1password-shell-plugins = {
      # enable 1Password shell plugins for bash, zsh, and fish shell
      enable = false;
      # the specified packages as well as 1Password CLI will be
      # automatically installed and configured to use shell plugins
      # plugins = with pkgs; [
      #   glab
      #   cachix
      #   nodePackages.vercel
      # ];
    };
    # Install firefox.
    firefox = {
      enable = true;
    };
    chromium.enable = true;
    java.enable = true;
    direnv.enable = true;
    ccache.enable = true;
    nix-ld.enable = true;
    git = {
      enable = true;
      lfs.enable = true;
    };
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
