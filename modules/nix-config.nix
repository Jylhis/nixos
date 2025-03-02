{ emacs-overlay, config, ... }:
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
    # Option definitions.
    # Define what other settings, services and resources should be active.
    # Usually these depend on whether a user of this module chose to "enable" it
    # using the "option" above.
    # Options for modules imported in "imports" can be set here.
    nix = {
      gc = {
        automatic = true;
        options = "--delete-older-than 14d";
        randomizedDelaySec = "45m";
      };
      optimise.automatic = true;

      settings = {
        extra-substituters = [
          "ssh://nix-ssh@lab"
        ];
        extra-trusted-public-keys = [
          "jylhis.com:2tR5lVPX2yBu38X3n2rkKYcXJdjbCcuiCgh4byzA21I="
        ];
        extra-trusted-substituters = [
          "ssh://nix-ssh@lab"
        ];

        auto-optimise-store = true;
        accept-flake-config = true;
        download-attempts = 2;
        connect-timeout = 5;
        fallback = true;
        tarball-ttl = 604800;
        keep-outputs = true;
        experimental-features = [
          "nix-command"
          "flakes"
        ];
        trusted-users = [
          config.users.users.root.name
          "@${config.users.groups.wheel.name}"
        ];
      };
    };

    nixpkgs = {
      overlays = [ emacs-overlay.overlay ];
      config = {
        allowUnfree = true;
      };
    };

  };
}
