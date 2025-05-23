{
  disko.devices = {
    disk = {
      main = {
        # Primary disk
        type = "disk";
        #device = "/dev/sda"; # <--- Adjust if needed
        content = {
          type = "gpt";
          partitions = {
            bios_boot = {
              size = "1M";
              type = "EF02"; # BIOS Boot for GRUB on GPT
            };
            # UEFI Boot Partition
            efi = {
              size = "500M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountOptions = [ "umask=0077" ];
                mountpoint = "/boot";
              };
            };
            # ZFS Partition
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
        # Secondary disk
        type = "disk";
        #device = "/dev/sdb"; # <--- Adjust if needed
        content = {
          type = "gpt";
          partitions = {
            bios_boot = {
              size = "1M";
              type = "EF02";
            };

            efi = {
              size = "500M";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountOptions = [ "umask=0077" ];
                # Some users also mirror the EFI partition or mount only one.
                # You could mount the second partition under /boot-efi2 if desired.
              };
            };
            # Entire disk for ZFS pool
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

    # ZFS mirror configuration
    zpool = {
      zroot = {
        type = "zpool";
        mode = "mirror"; # RAID-1
        # The partition numbers (sda2, sdb1, etc.) will match how Disko enumerates them
        # after creating the GPT partitions above. Adjust if needed (e.g. "/dev/sda2", "/dev/sdb2").
        #disks = [
        #  "/dev/sda2"
        #  "/dev/sdb1"
        #];

        # Common ZFS pool options: https://openzfs.github.io/openzfs-docs/Getting%20Started/NixOS/Root%20on%20ZFS.html
        options = {
          ashift = "12"; # Good default for modern disks (4K sectors)
        };

        # The "rootFsOptions" apply to the top-level dataset
        rootFsOptions = {
          canmount = "off"; # Don’t directly mount the top-level dataset
          mountpoint = "none";
          compression = "zstd";
          acltype = "posixacl";
          xattr = "sa";
          atime = "off";
        };

        # Datasets inside "zroot"
        datasets = {
          # Root filesystem
          root = {
            type = "zfs_fs";
            mountpoint = "/";
            options = {
              canmount = "on"; # The actual dataset that gets mounted at /
            };
          };

          # Nix store
          nix = {
            type = "zfs_fs";
            mountpoint = "/nix";
            options = {
              "com.sun:auto-snapshot" = "false"; # Example: disable auto-snapshot on /nix
            };
          };

          # Unencrypt after boot
          # zfs load-key zroot/data
          # zfs mount zroot/data
          data = {
            type = "zfs_fs";

            options = {
              mountpoint = "/data";
              encryption = "aes-256-gcm";
              keyformat = "passphrase";
              keylocation = "file:///tmp/disk.key";
              canmount = "noauto";
              aclinherit = "passthrough";
              recordsize = "1M";
            };
            postCreateHook = ''
              zfs set keylocation="prompt" zroot/data
            '';
          };

          # Unencrypt after boot
          # zfs load-key zroot/archive
          # zfs mount zroot/archive
          # archive = {
          #   type = "zfs_fs";
          #   options = {
          #     mountpoint = "/data/archive";
          #     encryption = "aes-256-gcm";
          #     keyformat = "passphrase";
          #     keylocation = "file:///tmp/disk.key";
          #     canmount = "noauto";
          #     aclinherit = "passthrough";
          #     dedup = "on";
          #     compression = "zstd-15";
          #     recordsize = "1M";
          #   };
          #   postCreateHook = ''
          #     zfs set keylocation="prompt" zroot/data
          #   '';
          # };
        };
      };
    };
  };
}
