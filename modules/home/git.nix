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
      delta.enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.gitFull;
      lfs.enable = lib.mkDefault true;
      enable = true;
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
