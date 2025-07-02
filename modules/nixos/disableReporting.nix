{ config, ... }:
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
  };
}
