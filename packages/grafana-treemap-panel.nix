{ grafanaPlugin, lib }:

grafanaPlugin rec {
  pname = "marcusolsson-treemap-panel";
  version = "2.0.1";
  zipHash = "sha256-2I0pnkOgeo7Q75yvEwerLUMKDBUt4Bkipru4YKoRQZY=";
  meta = with lib; {
    description = "A panel plugin for Grafana to visualize tree maps.";
    license = licenses.asl20;
    platforms = platforms.unix;
  };
}
