{
  lib,
  config,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.virtualisation.podman.enable {
    virtualisation = {
      containers.enable = true;
      podman = {
        dockerSocket.enable = true;
        defaultNetwork.settings.dns_enabled = true;
        dockerCompat = true;
      };
      docker.enable = false;
    };
    environment.systemPackages = with pkgs; [
      podman-compose
      docker-compose
      buildah
      skopeo
    ];
  };
}
