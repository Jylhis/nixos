{ lib, config, ... }:
{
  home.shellAliases = {
    open = "xdg-open";
    tree = "eza -T";
  };
  programs = {
    # on macOS, you probably don't need this
    bash = lib.mkIf config.programs.bash.enable {

      # sessionVariables = {
      #   PROMPT_DIRTRIM = 2; # # Automatically trim long paths in the prompt (requires Bash 4.x)
      # };

      bashrcExtra = ''
        	      vterm_printf() {
                        if [ -n "$TMUX" ] \
                          && { [ "''${TERM%%-*}" = "tmux" ] \
                              || [ "''${TERM%%-*}" = "screen" ]; }; then
                          # Tell tmux to pass the escape sequences through
                          printf "\ePtmux;\e\e]%s\007\e\\" "$1"
                        elif [ "''${TERM%%-*}" = "screen" ]; then
                          # GNU screen (screen, screen-256color, screen-256color-bce)
                          printf "\eP\e]%s\007\e\\" "$1"
                        else
                          printf "\e]%s\e\\" "$1"
                        fi
                     }
        	    '';
    };
  };
}
