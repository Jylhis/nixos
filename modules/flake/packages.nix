{ inputs, withSystem, ... }:
{
  systems = import inputs.systems;

  imports = [
    inputs.pkgs-by-name-for-flake-parts.flakeModule
  ];

  perSystem =
    { system, config, ... }:
    {
      pkgsDirectory = ../../packages;
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          (_final: _prev: {
            local = config.packages;
          })
          inputs.emacs-overlay.overlays.emacs
          #				inputs.emacs-overlays.emacs
        ];
      };
    };

  flake = {
    overlays.default =
      _final: prev:
      withSystem prev.stdenv.hostPlatform.system (
        { config, ... }:
        {
          local = config.packages;
        }
      );
  };
}
