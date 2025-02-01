{
  self,
  pkgs,
  config,
  home-manager,
  ...
}:
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
  ];

  options = {
    # Option declarations.
    # Declare what settings a user of this module can set.
    # Usually this includes a global "enable" option which defaults to false.
  };

  config = {

    home-manager.users.markus =
      {
        lib,
        #      config,
        #     nixosConfig,
        #    pkgs,
        ...
      }:
      {
        services = {
          emacs = {
            enable = true;
            package = self.outputs.packages.x86_64-linux.emacs-markus;
            client.enable = true;
            client.arguments = [
              "-c"
              "-a"
              "emacs"
            ];
            defaultEditor = true;
            socketActivation.enable = true;
          };
        };
        programs = {
          nix-index.enable = true;
          bash.enable = true;
          readline = {
            enable = true;
            variables = {
              colored-completion-prefix = true; # Enable coloured highlighting of completions
              completion-ignore-case = true; # Auto-complete files with the wrong case
              revert-all-at-newline = true; # Don't save edited commands until run
              show-all-if-ambiguous = true;
            };
          };
          git = {
            enable = true;
            userEmail = lib.mkForce "markus@jylhis.com";
            userName = "Jylhis";
            ignores = [
              "*~"
              "\#*\#"
              "*.elc"
              ".\#*"
              "[._]*.sw[a-p]"
            ];

          };
          emacs = {
            enable = true;
            package = self.outputs.packages.x86_64-linux.emacs-markus;
          };
          direnv.enable = true;
          starship.enable = true;
          ssh.enable = true;
          ssh.extraConfig = ''
            IdentityAgent ~/.1password/agent.sock
          '';

        };

        home = {
          keyboard = {
            options = [
              "ctrl:swapcaps"
              "terminate:ctrl_alt_bksp"
              "lv3:ralt_switch"
            ];
          };
          shellAliases = {
            ec = "emacsclient -t";
            eg = "emacsclient -c -a emacs";
            eb = "emacs -nw -Q";
            ebg = "emacs -Q";
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

        # Dev tools
        nix-diff
        ansible-language-server
        asm-lsp
        moreutils
        delve
        godef
        gopls
        nixd
        source-code-pro

        wget
        planify
        starship
        tailscale
        jetbrains.datagrip
        minio-client

        # CLI utils
        bat
        direnv
        nix-direnv
        git

        # Emacs support packages
        emacs-all-the-icons-fonts
        source-code-pro

        # Devtools (moved from emacs-markus)

        marksman
        ## Common
        ripgrep

        ## python
        python3Packages.python-lsp-server
        ruff

        ## Go
        # Golang
        go
        gopls
        godef
        delve

        # Nix
        statix
        nixd
        nixfmt-rfc-style

        # vala
        vala
        vala-lint

        # SQL
        sqls
        sqlint

        # Haskell
        ghc
        haskell-language-server
        cabal-install

        # C# dotnet
        dotnet-sdk
        csharp-ls

        # HTML + CSS
        stylelint

        # Config languages
        yamllint

        # Assembly
        nasm
        asm-lsp

        # Build tools
        cmake
        gnumake

        # Javascript & Typescript
        eslint
        typescript
        nodePackages.jsdoc

        # Docker
        hadolint
      ];
    };
  };

}
