{ lib, config, ... }:
{
  home.shellAliases = {
    open = "xdg-open";
    tree = "eza -T";
  };
  programs = {
    # on macOS, you probably don't need this
    bash = lib.mkIf config.programs.bash.enable {

      enableCompletion = true;
      enableVteIntegration = true;
      historyControl = [
        "ignoreboth"
        "erasedups"
      ];
      historyIgnore = [
        "$"
        "[ ]*"
        "exit"
        "ls"
        "bg"
        "fg"
        "history"
        "clear"
        "cd"
        "rm"
        "cat"
      ];
      shellOptions = [
        # Default
        "checkwinsize" # Checks window size after each command.
        "complete_fullquote"
        "expand_aliases"

        # Default from home manager
        "checkjobs"
        "extglob"
        "globstar"
        "histappend"

        # Other
        "cdspell" # Tries to fix minor errors in the directory spellings
        "dirspell"
        "shift_verbose"
        "cmdhist" # Save multi-line commands as one command
      ];
      # sessionVariables = {
      #   PROMPT_DIRTRIM = 2; # # Automatically trim long paths in the prompt (requires Bash 4.x)
      # };

      initExtra = ''
        		  # Enable history expansion with space
                  # E.g. typing !!<space> will replace the !! with your last command
                  bind Space:magic-space
      '';

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
    #  };
    readline = lib.mkIf config.programs.readline.enable {
      bindings = {
        # Up and down arrows search through the history for the characters before the cursor
        "\\e[A" = "history-search-backward";
        "\\e[B" = "history-search-forward";
      };

      variables = {
        colored-completion-prefix = true; # Enable coloured highlighting of completions
        completion-ignore-case = true; # Auto-complete files with the wrong case
        revert-all-at-newline = true; # Don't save edited commands until run
        show-all-if-ambiguous = true;

      };
    };

    # Better shell promt!
    starship = {
      enable = true;
    };
  };
}
