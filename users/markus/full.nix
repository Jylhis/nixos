{
  pkgs,
  lib,
  unstable,
  config,
  stylix,
  tinted-schemes,
  home-manager,
  sops-nix,
  ...
}:
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
    ./nixos.nix
    ./dev.nix
    ./music.nix
  ];

  options = {
    # Option declarations.
    # Declare what settings a user of this module can set.
    # Usually this includes a global "enable" option which defaults to false.
  };

  config = {

    # Wayland support for chrome
    environment.sessionVariables.NIXOS_OZONE_WL = "1";

    stylix = {
      enable = true;
      image = config.lib.stylix.pixel "base01";
      base16Scheme = "${tinted-schemes}/base16/catppuccin-latte.yaml";
      polarity = "light";
      fonts = {
        sizes = {
          terminal = 10;
        };
      };
    };

    specialisation.night.configuration = {
      stylix = {
        polarity = lib.mkForce "dark";
        base16Scheme = lib.mkForce "${tinted-schemes}/base16/catppuccin-macchiato.yaml";
      };
      home-manager.users.markus.dconf.settings."org/gnome/desktop/interface".color-scheme =
        lib.mkForce "prefer-dark";

    };

    home-manager.users.markus = _: {
      imports = [
        ./dconf.nix
        ./home.nix
        sops-nix.homeManagerModules.sops
      ];

      services.emacs.package = pkgs.emacs-markus;
      programs.emacs.package = pkgs.emacs-markus;

      home.stateVersion = "24.11";

    };

    users.users.markus = {
      extraGroups = [
        config.users.groups.networkmanager.name # For managing network connections
        config.users.groups.wheel.name # For sudo
      ];
      packages = with pkgs; [
        lnav
        ack
        texliveFull
        gnuplot
        hledger
        hledger-ui
        hledger-web

        yank
        rare-regex
        # General applications
        spotify
        cachix
        brave
        digikam

        sops
        age
        ssh-to-age
        git-agecrypt

        # Emacs spelling
        (aspellWithDicts (
          dicts: with dicts; [
            en
            en-computers
            en-science
            sv
            fi
            de
            fr
          ]
        ))

        signal-desktop
        unstable.protonmail-desktop

        jetbrains.datagrip

        todoist-electron
      ];
    };
  };

}
