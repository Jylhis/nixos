{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.emacs;
in
{
  options = {
    programs.emacs.userConfig = lib.mkOption {
      type = lib.types.path;
    };
  };
  config = lib.mkIf config.programs.emacs.enable {
    home = {
      file = {
        # Dynamically install all Emacs configuration files
        ".config/emacs" = {
          source = pkgs.runCommand "emacs-config" { } ''
            mkdir -p $out
            # Copy main configuration files
            cp -r ${cfg.userConfig}/* $out/
          '';
          recursive = true;
        };
      };
      shellAliases = {
        emc = "emacsclient -t -a emacs";
        emcg = "emacsclient -c -a emacs";
        emqg = "emacs -nw -Q";
        emq = "emacs -Q";
      };
      # TODO: move to fonts.nix
      packages =
        with pkgs.nerd-fonts;
        [
          dejavu-sans-mono
          envy-code-r
          fira-code
          fira-mono
          go-mono
          hack
          hasklug
          im-writing
          monaspace
          symbols-only
        ]
        ++ (with pkgs; [
          # Essential fonts only (4 instead of 10)
          jetbrains-mono
          inter
          source-code-pro
        ]);
    };

    services = {
      emacs = {
        enable = lib.mkDefault true;
        defaultEditor = true;

        client = {
          enable = lib.mkDefault true;
          arguments = [
            "-c"
            "-a"
            "emacs"
          ];
        };
        socketActivation.enable = true;
      };
    };
  };
}
