{
  lib,
  ...
}:
{
  imports = [ ../users.nix ];

  environment.pathsToLink = [ "share/thumbnailers" ];

  # REVIEW: Is this needed?
  services.udev.extraRules = lib.mkDefault ''ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{authorized}=="0", ATTR{authorized}="1"'';

}
