{ pkgs, ... }:
{
  virtualisation = {
    containers.enable = true;
    podman = {
      enable = true;
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
}
