{
  lib,
  pkgs,
  config,
  ...
}:
{
  # Nix packages to install to $HOME
  #
  # Search for packages here: https://search.nixos.org/packages
  home.packages = with pkgs; [
    omnix
    just

    # Unix tools
    ripgrep # Better `grep`
    fd
    sd
    tree
    gnumake
    unzip
    moreutils

    # Nix dev
    cachix
    nil # Nix language server
    nix-info
    nixpkgs-fmt

    # On ubuntu, we need this less for `man home-configuration.nix`'s pager to
    # work.
    less

  ];

  # Programs natively supported by home-manager.
  # They can be configured in `programs.*` instead of using home.packages.
  programs = {
    # Better `cat`
    bat.enable = true;
    # Type `<ctrl> + r` to fuzzy search your shell history
    # Keybinds:
    # Change dir widget: ALT-C
    # file widget: CTRL-T
    # history widget: CTRL-R
    fzf.enable = true;
    jq.enable = true;
    # Install btop https://github.com/aristocratos/btop
    btop.enable = true;

    eza = {
      enable = true;
      colors = "auto";
      icons = "auto";
      git = true;
    };
    zoxide.enable = true;
    fd.enable = true;
    nh = {
      enable = true;
      flake = lib.mkDefault config.me.flakePath;
    };
    ripgrep = {
      enable = true;
      arguments = lib.mkDefault [
        "--smart-case"
        # --type-add 'foo:*.foo'
      ];
    };

  };
}
