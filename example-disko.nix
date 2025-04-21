{
  # TODO:
  # https://www.debian.org/doc/manuals/securing-debian-manual/ch03s02.en.html
  # separate partition for /home /tmp /var/tmp
  # separate part: /var
  # Partition /boot # NOTE: Big enought for nix generations
  # /nix
  # desktop filesystem xfs vs ext4?
  # server filesystem?
  # TODO: Mounting https://www.debian.org/doc/manuals/securing-debian-manual/ch04s10.en.html
  disko.devices = {
    disk = {
      main = {
        device = "/dev/disk/by-id/some-disk-id";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "500M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
