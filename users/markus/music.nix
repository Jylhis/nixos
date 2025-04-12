{
  config,
  pkgs,
  musnix,
  ...
}:
{
  imports = [
    musnix.nixosModules.musnix
  ];
  config = {
    musnix.enable = true;
    users.users.markus = {
      extraGroups = [ "audio" ];
      packages = with pkgs; [
        lmms
        audacity
        ardour
        muse
        renoise
        musescore
        qtractor
        rosegarden
        guitarix
        hydrogen
        mixxx
        bespokesynth
      ];
    };
  };
}
