{ inputs, withSystem, ... }:
{
  systems = import inputs.systems;

  imports = [
    inputs.pkgs-by-name-for-flake-parts.flakeModule
  ];

  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.self.overlays.default
          inputs.emacs-overlay.overlays.emacs
          #				inputs.emacs-overlays.emacs
        ];
      };
      pkgsDirectory = ../../packages;
    };

  flake = {
    overlays.default =
      _final: prev:
      withSystem prev.stdenv.hostPlatform.system (
        { config, ... }:
        {
          jylhis = config.packages;
        }
      );
  };
}
