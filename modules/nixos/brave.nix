{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    (brave.override {
      commandLineArgs = [
        "--enable-features=AcceleratedVideoEncoder,TouchpadOverscrollHistoryNavigation"
        "--ignore-gpu-blocklist"
        "--enable-zero-copy"
        "--ozone-platform=wayland"
        "--ozone-platform-hint=wayland"
      ];
    })
  ];

}
