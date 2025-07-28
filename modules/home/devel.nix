_:
{
  programs = {
    ccache.enable = true;
    nix-ld.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };
}
