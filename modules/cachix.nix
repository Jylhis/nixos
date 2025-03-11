{ pkgs, ... }:
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
    nix.settings = {

      substituters = [
        "https://nix-community.cachix.org"
        "https://jylhis-nixos.cachix.org"
      ];
      trusted-substituters = [

        "https://nix-community.cachix.org"
        "https://jylhis-nixos.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "jylhis-nixos.cachix.org-1:Sk7hqPdA7V0TJvwQakraPOtdPHd4vMrkunpUxub831E="
      ];
    };

    environment.systemPackages = [
      pkgs.cachix
    ];
  };
}
