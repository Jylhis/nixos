{
  config = {
    #nix.nixPath = [ "nixpkgs=${nixpkgs}" ];
    programs = {
      ccache.enable = true;
      direnv.enable = true;
    };
  };
}
