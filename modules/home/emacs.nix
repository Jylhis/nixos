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
        ".config/emacs/init.el".source = ../../sources/emacs/init.el;
        # Install compiled elisp files
        # ".config/emacs" = {
        #   source = pkgs.runCommand "emacs-config" {} ''
        #     mkdir -p $out/config-lib
        #     cp ${../../sources/emacs}/*.el* $out/
        #     cp ${../../sources/emacs}/config-lib/*.el* $out/config-lib/
        #   '';
        #};
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
