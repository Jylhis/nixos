{
  config,
  lib,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.programs.emacs.enable {
    home = {
      file = {
        # Dynamically install all Emacs configuration files
        ".config/emacs" = {
          source = pkgs.runCommand "emacs-config" { } ''
            mkdir -p $out/config $out/lisp

            # Copy main configuration files
            cp ${../../sources/emacs/early-init.el} $out/early-init.el
            cp ${../../sources/emacs/init.el} $out/init.el

            # Copy modular configuration
            cp ${../../sources/emacs/config}/*.el $out/config/

            # Copy utility functions
            cp ${../../sources/emacs/lisp}/*.el $out/lisp/
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
      packages = with pkgs.nerd-fonts; [
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
      ];
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
