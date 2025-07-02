{ config, pkgs, ... }:
{
  home.shellAliases = {
    g = "git";
    lg = "lazygit";
  };

  # https://nixos.asia/en/git
  programs = {
    git = {
      delta.enable = true;
      package = pkgs.gitFull;
      lfs.enable = true;
      enable = true;
      userName = config.me.fullname;
      userEmail = config.me.email;
      ignores = [
        "*~"
        "*.swp"
      ];
      aliases = {
        ci = "commit";
      };
      extraConfig = {
        init.defaultBranch = "main";
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
