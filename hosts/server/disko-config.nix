# USAGE in your configuration.nix.
# Update devices to match your hardware.
# {
#  imports = [ ./disko-config.nix ];
#  disko.devices.disk.main.device = "/dev/sda";
# }

{
  disko.devices = {
    disk = {
      main = {
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            boot = {
              size = "1M";
              type = "EF02"; # for grub MBR
              priority = 1; # Needs to be first partition
            };
            ESP = {
              size = "500M";
              type = "EF00";

              content = {
                type = "filesystem";
                format = "vfat";
                mountOptions = [ "umask=0077" ];
                mountpoint = "/boot";
              };
            };
            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "zroot";

              };
            };
          };
        };
      };
      secondary = {
        type = "disk";
        content = {
          type = "gpt";
          partitions = {

            zfs = {
              size = "100%";
              content = {
                type = "zfs";
                pool = "zroot";

              };
            };
          };
        };
      };
    };
    zpool = {
      zroot = {
        type = "zpool";
        mode = "mirror"; # RAID1
        # Workaround: cannot import 'zroot': I/O error in disko tests
        #options.cachefile = "none";
        rootFsOptions = {
          # https://wiki.archlinux.org/title/Install_Arch_Linux_on_ZFS
          # acltype = "posixacl";
          # atime = "off";
          compression = "zstd";
          # mountpoint = "none";
          # xattr = "sa";
        };
        #options.ashift = "12";
        mountpoint = "/";

        datasets = {

          "local/nix" = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options."com.sun:auto-snapshot" = "false";
          };

          "data" = {
            type = "zfs_fs";
            mountpoint = "/data";
            options = {
              encryption = "aes-256-gcm";
              keyformat = "passphrase";
              keylocation = "file:///tmp/disk.key";
            };

          };

        };
      };
    };
  };
}
