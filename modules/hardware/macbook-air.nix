{
  self,
  lib,
  nixos-hardware,
  ...
}:
{
  imports = [
    self.nixosModules.hardware-apple
    nixos-hardware.nixosModules.common-cpu-intel
    nixos-hardware.nixosModules.common-pc-laptop
    nixos-hardware.nixosModules.common-pc-laptop-ssd
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

    boot = {
      kernelModules = [ "kvm-intel" ];
      initrd = {
        availableKernelModules = [
          "xhci_pci"
          "nvme"
          "usbhid"
          "uas"
          "sd_mod"
        ];
      };
    };
    powerManagement.enable = true;

    hardware-apple.enableFirmware = true;
    hardware-apple.firmware = self.outputs.packages.x86_64-linux.brcm-firmware.override {
      name = "macbook-air-firmware.tar";
      hash = "sha256-YEDh9tljjDSReQaoNwiuzODAiYRAAt1ls77XBXRaJUs=";
    };

    services = {
      thermald.enable = true;
    };
  };
}
