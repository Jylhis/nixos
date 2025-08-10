# Desktop NixOS configuration module.
#
# This module provides a complete desktop environment configuration
# suitable for workstations and personal computers. It includes
# graphical desktop environment, fonts, and peripheral support.
{
  imports = [
    ./common.nix
    ./plasma.nix
    ./fonts.nix
    ../_1password.nix
    ../default.nix
    ../logitech.nix
    ../users.nix
    ../plymouth.nix
  ];
}
