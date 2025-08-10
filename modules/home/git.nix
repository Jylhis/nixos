# Git configuration module for Home Manager.
#
# Provides comprehensive Git configuration with modern features including
# delta for improved diffs, Git LFS support, and optimized performance settings.
# Also includes lazygit for terminal-based Git interface.
#
# Features:
# - Delta integration for enhanced diff viewing
# - Performance optimizations (fsmonitor, commit graph)
# - Comprehensive gitignore patterns for common editors
# - Modern merge and rebase configurations
# - SOPS integration for encrypted file diffs
#
# Configuration is conditional on programs.git.enable being set.
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
