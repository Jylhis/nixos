# This is specifically for Apple Mac Mini 2018 with i7-8700B
{
  self,
  pkgs,
  lib,
  nixos-hardware,
  config,
  ...
}:
{
  imports = [
    nixos-hardware.nixosModules.common-cpu-intel
    nixos-hardware.nixosModules.common-pc
    nixos-hardware.nixosModules.common-pc-ssd
    self.nixosModules.apple-hardware
    # Paths to other modules.
    # Compose this module out of smaller ones.
  ];

  options = {
    # Option declarations.
    # Declare what settings a user of this module can set.
    # Usually this includes a global "enable" option which defaults to false.
  };

  config = {
    nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
    hardware.intelgpu.vaapiDriver = "intel-media-driver";
    hardware.graphics.extraPackages = lib.optionals config.hardware.graphics.enable [
      pkgs.intel-media-driver
      pkgs.vaapiVdpau
      pkgs.intel-compute-runtime # OpenCL filter support (hardware tonemapping and subtitle burn-in)
      pkgs.intel-media-sdk # QSV up to 11th gen
    ];
    programs.firefox.preferences =
      lib.mkIf (config.hardware.graphics.enable && config.programs.firefox.enable)
        {
          "media.ffmpeg.vaapi.enabled" = true;
        };
    boot = {
      kernelModules = [ "kvm-intel" ];
      kernelParams = [
        "nosgx"
        "i915.enable_guc=2"
      ];
      initrd = {
        availableKernelModules = [
          "xhci_pci"
          "nvme"
          "usbhid"
          "usb_storage"
          "uas"
          "sd_mod"
        ];
      };
    };
    apple-hardware.enableFirmware = false;
    apple-hardware.firmware = self.outputs.packages.x86_64-linux.brcm-firmware.override {
      name = "firmware-mac-mini.tar";
      hash = "sha256-nQmzaCAIcApl0ihSz/dV2z8iYPTGKBo04+Wxr3Uh7hc=";
    };
  };
}
