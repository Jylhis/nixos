{
  lib,
  config,
  pkgs,
  ...
}:
let
  zfsCompatibleKernelPackages = lib.filterAttrs (
    name: kernelPackages:
    (builtins.match "linux_[0-9]+_[0-9]+" name) != null
    && (builtins.tryEval kernelPackages).success
    && (!kernelPackages.${config.boot.zfs.package.kernelModuleAttribute}.meta.broken)
  ) pkgs.linuxKernel.packages;

  latestKernelPackage = lib.last (
    lib.sort (a: b: (lib.versionOlder a.kernel.version b.kernel.version)) (
      builtins.attrValues zfsCompatibleKernelPackages
    )
  );
in
{
  config = lib.mkIf config.boot.zfs.enabled {
    boot = {
      zfs = {
        devNodes = lib.mkDefault "/dev";
        forceImportRoot = lib.mkDefault false;
        forceImportAll = lib.mkDefault false;
        passwordTimeout = lib.mkDefault 60;
        requestEncryptionCredentials = lib.mkDefault false;
      };
      kernelPackages = latestKernelPackage;

      loader = {
        grub.zfsSupport = lib.mkDefault true;
      };
    };
    services = {
      zfs = {
        autoSnapshot.enable = lib.mkDefault true;
        # defaults to 12, which is a bit much given how much data is written
        autoSnapshot.monthly = lib.mkDefault 1;
        autoScrub.enable = lib.mkDefault true;
      };
    };

  };

}
