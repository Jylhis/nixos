{ lib, config, ... }:
{
  config = {

    services = {
      alloy.extraFlags = [ "--disable-reporting" ];
      loki.extraFlags = [ "-reporting.enabled=0" ];
      grafana.settings = {
        analytics.reporting_enabled = false;
        analytics.feedback_links_enabled = false;
        security.disable_gravatar = true;
      };

    };
    systemd.services = {
      syncthing = lib.mkIf config.services.syncthing.enable {
        serviceConfig.UMask = "0002";
        environment.STNODEFAULTFOLDER = "true"; # Don't create default ~/Sync folder
      };
    };
  };
}
