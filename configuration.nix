# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  lib,
  pkgs,
  _1password-shell-plugins,
  ...
}: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    _1password-shell-plugins.nixosModules.default
  ];

  nix.settings.experimental-features = ["nix-command" "flakes"];

  # Bootloader.
  boot = {
    loader.systemd-boot.enable = true;

    loader.efi.canTouchEfiVariables = true;
    kernelPackages = pkgs.linuxPackages_latest;
  };

  networking = {
    hostName = "nixos"; # Define your hostname.
    networkmanager.enable = true;
  };

  # Set your time zone.
  time.timeZone = "Europe/Zurich";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  services = {
    xserver.enable = true;

    xserver.displayManager.gdm.enable = true;
    xserver.desktopManager.gnome.enable = true;

    # Set layout in GNOME
    xserver.desktopManager.gnome = {
      extraGSettingsOverrides = ''
        [org.gnome.desktop.input-sources]
        sources=[('xkb', 'us'), ('xkb', 'fi')]
      '';
      #extraGSettingsOverridePackages = [
      #  pkgs.gsettings-desktop-schemas
      #];
    };

    # Configure keymap in X11
    xserver.xkb = {
      layout = "us,fi";
      variant = "";
    };

    # Enable CUPS to print documents.
    printing.enable = true;

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    emacs.enable = true;

    # Enable the GNOME Desktop Environment.
    emacs.package = with pkgs; (
      (emacsPackagesFor emacs).emacsWithPackages (
        epkgs:
          with epkgs; [
            nix-mode
            markdown-mode
            markdown-toc
            solaire-mode
            doom-themes
            treemacs
            treemacs-projectile
            treemacs-magit
            treemacs-icons-dired
            treemacs-all-the-icons
            json-mode
            json-snatcher
            flycheck
            exec-path-from-shell
            pyenv-mode
            lsp-pyright
            python-black
            blacken
            python-insert-docstring
            python
            cython-mode
            rust-mode
            go-mode
            editorconfig
            editorconfig-generate
            editorconfig-domain-specific
            editorconfig-custom-majormode
            yasnippet
            yasnippet-snippets
            magit
            system-packages
            use-package-ensure-system-package
            auto-package-update
            company
            diminish
            dashboard
            tree-sitter
            tsc
            tree-sitter-langs
            projectile
            projectile-ripgrep
            google-c-style
            modern-cpp-font-lock
            cmake-ide
            cmake-mode
            cmake-font-lock
            sphinx-doc
            sphinx-mode
            highlight-indentation
            yaml-mode
            dash
            diff-hl
            copilot
            jsonrpc
            dtrt-indent
            move-text
            consult
            helpful
            marginalia
            vertico
            which-key
            gitlab-ci-mode
            docker-compose-mode
            dockerfile-mode
            lsp-treemacs
            dap-mode
            lsp-ui
            lsp-mode
            powershell
          ]
      )
    );
  };

  environment = {
    gnome.excludePackages =
      (with pkgs; [
        rhythmbox
        epiphany
      ])
      ++ (with pkgs.gnome; [
        geary
        gnome-weather
        gnome-maps
        gnome-music
      ]);

    # List packages installed in system profile. To search, run:
    # $ nix search wget
    systemPackages = with pkgs; [
      vim
      bat
      ripgrep
      eza
      delta
      dust
      duf
      fd
      zoxide
      glances
      curlie
      jq
      sd
      gnumake
      git
    ];
  };

  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.markus = {
    isNormalUser = true;
    description = "Markus";
    extraGroups = ["networkmanager" "wheel"];
    packages = with pkgs; [
      spotify
      alacritty
      signal-desktop

      nixd
      gopls
      godef
      delve
      source-code-pro
      clang
      asm-lsp
      ansible-language-server
      emacs-all-the-icons-fonts
    ];
  };

  programs = {
    _1password.enable = true;
    _1password-gui = {
      enable = true;
      polkitPolicyOwners = ["markus"];
    };
    _1password-shell-plugins = {
      # enable 1Password shell plugins for bash, zsh, and fish shell
      enable = true;
      # the specified packages as well as 1Password CLI will be
      # automatically installed and configured to use shell plugins
      plugins = with pkgs; [gh awscli2 cachix];
    };
    # Install firefox.
    firefox.enable = true;
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "24.05"; # Did you read the comment?
}
