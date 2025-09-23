{
  lib,
  pkgs,
  config,
  ...
}:
let
  backend = "docker";
  tmpEnable = true;
  inherit ((import ../../lib { inherit lib; })) mapListToAttrs;
in
{
  imports = [
    ./users.nix
  ];
  config = lib.mkIf tmpEnable {
    virtualisation = {
      containers.enable = true;
      docker.enable = backend == "docker";
      podman = lib.mkIf (backend == "podman") {
        dockerSocket.enable = true;
        defaultNetwork.settings.dns_enabled = true;
        dockerCompat = true;
      };
    };

    environment.systemPackages = with pkgs; [
      docker-compose
      buildah
      skopeo
    ];

    users.users = mapListToAttrs config.myusers (_name: {
      extraGroups = [
        config.users.groups."${backend}".name
      ];
    });
  };

}
