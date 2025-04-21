{
  pkgs,
  #  nil-lsp,
  ...
}:
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
    ./nixos.nix
    ../../modules/roles/developer
  ];

  options = {
    # Option declarations.
    # Declare what settings a user of this module can set.
    # Usually this includes a global "enable" option which defaults to false.
  };

  config = {

    jylhis.role.developer = {
      enable = true;
      languages = {
        go = true;
        cpp = true;
        nix = true;
        python = true;
        haskell = true;
        bash = true;
      };
    };

    environment.systemPackages = with pkgs; [
      devdocs-desktop

      # CLI utils
      moreutils
      btop
      htop

      # Python
      python3Packages.python-lsp-server
      python3Packages.python-lsp-ruff
      python3Packages.pylsp-rope
      python3Packages.pylsp-mypy

      # SQL
      sqls
      sqlint

      # HTML + CSS
      stylelint

      # Config languages
      yamllint
      nodePackages.jsonlint
      nodePackages.vscode-json-languageserver
      yaml-language-server

      # Build tools
      cmake-language-server

      # Docker
      dockerfile-language-server-nodejs
      hadolint

      marksman
      # Extra stuff
      emacs-all-the-icons-fonts
      source-code-pro

    ];
  };
}
