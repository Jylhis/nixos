{
  lib,
  pkgs,
  config,
  ...
}:
{

  config = {

    manual = {
      html.enable = true;
      manpages.enable = true;
    };

    home = {
      language = {
        address = "de_CH.UTF-8";
        base = "en_US.UTF-8";
        collate = "de_CH.UTF-8";
        ctype = "de_CH.UTF-8";
        measurement = "de_CH.UTF-8";
        messages = "en_US.UTF-8";
        monetary = "de_CH.UTF-8";
        name = "de_CH.UTF-8";
        numeric = "de_CH.UTF-8";
        paper = "de_CH.UTF-8";
        telephone = "de_CH.UTF-8";
        time = "fi_FI.UTF-8";
      };
      keyboard = {
        options = [
          "ctrl:swapcaps"
          "terminate:ctrl_alt_bksp"
          "lv3:ralt_switch"
        ];
      };
      shellAliases = {
        emc = "emacsclient -t";
        emcg = "emacsclient -c -a emacs";
        emqg = "emacs -nw -Q";
        emq = "emacs -Q";
        open = "xdg-open";
        tree = "eza -T";
      };

    };

    stylix = {
      targets = {
        emacs.enable = false;
      };
    };

    sops = {
      age = {
        keyFile = "${config.home.homeDirectory}/.config/sops/age/keys.txt";
      };
      defaultSopsFile = ./secrets.yaml;
      secrets = {
        #hello = {};
      };

    };

    services = {
      syncthing = {
        enable = true;
        tray.enable = true;
      };
      emacs = {
        enable = true;
        defaultEditor = true;

        client = {
          enable = true;
          arguments = [
            "-c"
            "-a"
            "emacs"
          ];
        };
        socketActivation.enable = true;
      };
    };

    programs = {

      readline = {
        enable = true;
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
      git = {
        enable = true;
        difftastic = {
          enable = false;
          color = "auto";
          enableAsDifftool = true;
        };
        delta.enable = true;
        package = pkgs.gitFull;
        lfs = {
          enable = true;
        };
        signing.format = "ssh";
        # maintenance = {
        #   # git maintenance start
        #   enable = true;
        #   repositories = [
        #     "${config.home.homeDirectory}/Developer/j10s"
        #   ];
        # };

        userEmail = lib.mkForce "markus@jylhis.com";
        userName = "Jylhis";
        ignores = [
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
        extraConfig = {
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
          git-agecrypt =
            if builtins.hasAttr "git-agecrypt" pkgs then
              {
                "config".identity = config.sops.age.keyFile;
              }
            else
              null;

        };
      };
      emacs = {
        enable = true;

      };
      neovim = {
        enable = true;
        plugins = with pkgs.vimPlugins; [
          vim-nix
          vim-lastplace
        ];
        extraPackages = [
          #pkgs.vim-language-server
          pkgs.xclip # X11
          # pkgs.wl-copy # Wayland
        ];
        defaultEditor = false;
        viAlias = true;
        vimAlias = true;
        extraConfig = ''
                    " your custom vimrc
                    set nocompatible
                    set backspace=indent,eol,start
                    " Turn on syntax highlighting by default
                    syntax on
          	  set number
                    set cursorline
          	  set diffopt+=iwhite
                    set wildignorecase
          	  set copyindent
                    set smarttab
          	  set autoindent
                    set smartindent

                    set hlsearch "when there is a previous search pattern, highlight all its matches
                    set incsearch "while typing a search command, show immediately where the so far typed pattern matches
                    set ignorecase "ignore case in search patterns
                    set smartcase "override the 'ignorecase' option if the search pattern contains uppercase characters
                    set gdefault "imply global for new searches

                    " Stuff


                    " disable vi compatibility
                    set nocompatible

                    set encoding=utf-8
          	  filetype plugin indent on
        '';
      };
      direnv = {
        enable = true;
        silent = true;
        config = {
          global = {
            hide_env_diff = true;
          };
        };
        nix-direnv.enable = true;

      };
      starship.enable = true;
      ssh = {
        enable = true;
        extraConfig = ''
          IdentityAgent ~/.1password/agent.sock
          Include ~/.ssh/1Password/config
        '';
      };

      btop.enable = true;

      bat.enable = true;
      fzf = {
        # Keybinds:
        # Change dir widget: ALT-C
        # file widget: CTRL-T
        # history widget: CTRL-R
        enable = true;

      };
      eza = {
        enable = true;
        colors = "auto";
        icons = "auto";
        git = true;
      };
      fd.enable = true;
      zoxide.enable = true;
      nh = {
        enable = true;
        flake = "${config.home.homeDirectory}/Developer/root";
      };

      ripgrep = {
        enable = true;
        arguments = [
          "--smart-case"
          # --type-add 'foo:*.foo'
        ];
      };

      bash = {
        enable = true;
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
        sessionVariables = {
          PROMPT_DIRTRIM = 2; # # Automatically trim long paths in the prompt (requires Bash 4.x)
        };

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

    };

  };
}
