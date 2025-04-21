{ ... }:
{
  imports = [
    ./common.nix
    ./services.nix
    ./subtituters.nix
    ./zfs.nix
  ];

  options = {
    jylhis = {
      role = {

      };
    };

  };

}
