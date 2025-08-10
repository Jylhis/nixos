# Flake modules default import
# This module automatically imports all other modules in this directory
# except for this default.nix file itself.
{
  imports =
    with builtins;
    map (fn: ./${fn}) (filter (fn: fn != "default.nix") (attrNames (readDir ./.)));

}
