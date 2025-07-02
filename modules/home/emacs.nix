{ config, lib, ... }:
{
  home = {
    file = {
      ".config/emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink ../../configs/init.el;
    };
    shellAliases = {
      emc = "emacsclient -t";
      emcg = "emacsclient -c -a emacs";
      emqg = "emacs -nw -Q";
      emq = "emacs -Q";
    };
  };
  programs.emacs.enable = true;
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
}
