{
  lib,
  config,
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
      lfs.enable = lib.mkDefault true;

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

    };
  };

}
