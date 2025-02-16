{
  self,
  pkgs,
  config,
  home-manager,
  sops-nix,
  ...
}:
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.

    ../../modules/markus-dev.nix
  ];

  options = {
    # Option declarations.
    # Declare what settings a user of this module can set.
    # Usually this includes a global "enable" option which defaults to false.
  };

  config = {

    environment.systemPackages = with pkgs; [
      dconf2nix
      # Emacs spelling
      (aspellWithDicts (
        dicts: with dicts; [
          en
          en-computers
          en-science
          sv
          fi
          de
          fr
        ]
      ))
    ];
    home-manager.users.markus =
      {
        lib,
        sops,
        ...
      }:
      {
        sops = {
          age = {
            keyFile = "${config.users.users.markus.home}/.config/sops/age/keys.txt";
          };
          defaultSopsFile = ./secrets.yaml;
          secrets = {
            #hello = {};
          };

        };
        imports = [
          ./dconf.nix
          sops-nix.homeManagerModules.sops
        ];

        services = {
          emacs = {
            enable = true;
            defaultEditor = true;
            package = self.outputs.packages.x86_64-linux.emacs-markus;
            client.enable = true;
            client.arguments = [
              "-c"
              "-a"
              "emacs"
            ];
            socketActivation.enable = true;
          };
        };
        programs = {

          bash = {
            enable = true;
            enableCompletion = true;
            enableVteIntegration = true;
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
            historyControl = [
              "ignoreboth"
              "erasedups"
            ];
            historyIgnore = [
              "ls"
              "cd"
              "exit"
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
            ];
          };

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
            lfs = {
              enable = true;
            };
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
              color = {
                ui = true;
              };
              pull.rebase = true;
              diff = {
                "sops-decrypt".textconv = "sops decrypt";
              };
              git-agecrypt =
                if builtins.hasAttr "git-agecrypt" pkgs then
                  {
                    "config".identity = config.home-manager.users.markus.sops.age.keyFile;
                  }
                else
                  null;

            };
          };
          emacs = {
            enable = true;
            package = self.outputs.packages.x86_64-linux.emacs-markus;

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
          direnv.enable = true;
          starship.enable = true;
          ssh.enable = true;
          ssh.extraConfig = ''
            IdentityAgent ~/.1password/agent.sock
          '';

        };

        home = {
          language = {
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
            time = "de_CH.UTF-8";
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
          };

          stateVersion = "24.11";
        };
      };

    # Option definitions.
    # Define what other settings, services and resources should be active.
    # Usually these depend on whether a user of this module chose to "enable" it
    # using the "option" above.
    # Options for modules imported in "imports" can be set here.
    users.users.markus = {
      isNormalUser = true;
      description = "Markus";
      extraGroups = [
        config.users.groups.networkmanager.name # For managing network connections
        config.users.groups.wheel.name # For sudo

      ];

      packages = with pkgs; [
        # General applications
        spotify
        signal-desktop

        jetbrains.datagrip

        planify
        starship

      ];
    };
  };

}
