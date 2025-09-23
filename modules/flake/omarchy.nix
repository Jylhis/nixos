_: {
  # debug = true; # https://flake.parts/debug
  flake = {
    nixosModules = {
      omarchy = ../nixos/omarchy;
    };
    homeModules = {
      omarchy = ../home/omarchy;
    };
  };
}
