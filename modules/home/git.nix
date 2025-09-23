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

  programs = {
    git = lib.mkIf config.programs.git.enable {
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
        pclean = "clean --exclude='.aider*' --exclude='**/.dir-locals-2.el' --exclude='**/.envrc.private' --exclude='.pre-commit-config.yaml' -dxf";
      };

      extraConfig = {
        init.defaultBranch = lib.mkDefault "main";
        core = {
          untrackedcache = lib.mkDefault true;
          fsmonitor = lib.mkDefault true;
        };
        merge = {
          conflictStyle = lib.mkDefault "zdiff3";
        };
        rebase = {
          updateRefs = lib.mkDefault true;
        };
        color = {
          ui = lib.mkDefault true;
        };
        column = {
          ui = lib.mkDefault "auto";
        };
        fetch = {
          writeCommitGraph = lib.mkDefault true;
        };
        branch = {
          sort = lib.mkDefault "-committerdate";
        };
        pull.rebase = lib.mkDefault true;
        diff = {
          colorMoved = lib.mkDefault "zebra";
          colorMovedWS = lib.mkDefault "ignore-space-at-eol";
          "sops-decrypt".textconv = lib.mkDefault "sops decrypt";
        };
        rerere = {
          enabled = lib.mkDefault true;
        };
      };
    };
    lazygit.enable = lib.mkDefault true;
  };

}
