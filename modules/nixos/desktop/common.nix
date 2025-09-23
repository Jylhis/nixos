{
  lib,
  pkgs,
  ...
}:
{
  imports = [ ../users.nix ];

  programs.nix-ld.enable = true;
  hardware = {
    enableAllFirmware = true;
  };
  nix.daemonCPUSchedPolicy = "idle";

  # TODO: Organize

  services.earlyoom.enable = true;

  boot.kernelParams = [ "mitigations=off" ];
  time.timeZone = lib.mkDefault "Europe/Zurich";

  documentation = {
    doc.enable = true;
    nixos.enable = true;
    man.enable = true;
    dev.enable = true;
    info.enable = true;
  };
  # Enable HEIC support
  environment.systemPackages = [
    pkgs.libheif
    pkgs.libheif.out
    # attempt to fix following errors when plugging in usb stick:
    # .gvfsd-wsdd-wra[4217]: Failed to spawn the wsdd daemon: Failed to execute child process “wsdd” (No such file or directory)
    # Couldn't create directory monitor on wsdd:///. Error: Automount failed: Failed to spawn the underlying wsdd daemon.
    # Source: https://forums.linuxmint.com/viewtopic.php?p=2526298&sid=1221d1580704693bbaaf3be6de57f10c#p2526298
    pkgs.wsdd

    # Documentation
    pkgs.man-pages
    pkgs.man-pages-posix
    pkgs.linux-doc
    # pkgs.linux-manual
    pkgs.clang-manpages
    pkgs.stdmanpages
    # pkgs.devhelp
    #    pkgs.devdocs-desktop
    pkgs.zeal
    #    pkgs.wikiman
    pkgs.manix
    # pkgs.xorg-docs
  ];
  environment.pathsToLink = [ "share/thumbnailers" ];

  # REVIEW: Is this needed?
  services.udev.extraRules = lib.mkDefault ''ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{authorized}=="0", ATTR{authorized}="1"'';

}
