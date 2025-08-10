# Home Manager modules default import.
#
# This module automatically imports all other Home Manager modules
# in this directory, providing a single entry point for user-specific
# configurations including shell, development tools, applications,
# and personal settings.
{
  imports =
    with builtins;
    map (fn: ./${fn}) (filter (fn: fn != "default.nix") (attrNames (readDir ./.)));
}
