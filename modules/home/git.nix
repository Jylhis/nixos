{
  lib,
  config,
  pkgs,
  ...
}:
{
  home.shellAliases = {
    g = "git";
    lg = "lazygit";
  };

  # https://nixos.asia/en/git
  programs = {
    git = {
      enable = true;
      delta.enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.gitFull;
      lfs.enable = lib.mkDefault true;
      userName = config.me.fullname;
      userEmail = config.me.email;
      ignores = lib.mkDefault [
        "*~"
        # Emacs
        "\#*\#"
        "*.elc"
        ".\#*"
        # Org-mode
        ".org-id-locations"
        "*_archive"
        # Intellij
        "*.iml"
        "*.ipr"
        "*.iws"
        ".idea/"
        # VIM
        ".*.s[a-w][a-z]"
        "*.un~"
        "Session.vim"
        ".netrwhist"

      ];

      aliases = {
        pclean = "clean --exclude='**/.dir-locals-2.el' --exclude='**/.envrc.private' --exclude='.pre-commit-config.yaml' -dxf";
      };

      extraConfig = {
        init.defaultBranch = lib.mkDefault "main";
        core = {
          untrackedcache = true;
          fsmonitor = true;
        };
        merge = {
          conflictStyle = "zdiff3";
        };
        rebase = {
          updateRefs = true;
        };
        color = {
          ui = true;
        };
        column = {
          ui = "auto";
        };
        fetch = {
          writeCommitGraph = true;
        };
        branch = {
          sort = "-committerdate";
        };
        pull.rebase = true;
        diff = {
          colorMoved = "zebra";
          colorMovedWS = "ignore-space-at-eol";
          "sops-decrypt".textconv = "sops decrypt";
        };
        rerere = {
          enabled = true;
        };
      };
    };
    lazygit.enable = true;
  };

}
