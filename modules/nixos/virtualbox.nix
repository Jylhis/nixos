{ lib, config, ... }:

let
  mapListToAttrs =
    m: f:
    lib.listToAttrs (
      map (name: {
        inherit name;
        value = f name;
      }) m
    );
in
{
  config = lib.mkIf config.virtualisation.virtualbox.host.enable {
    # Add all users from `myusers` to `vboxusers` and `dialout` groups
    users.users = mapListToAttrs config.myusers (name: {
      extraGroups = lib.unique (
        lib.optionals (config.users.users.${name}.extraGroups or [ ]) [
          config.users.groups.vboxusers.name
          config.users.groups.dialout.name # Serial console access
        ]
      );
    });
  };
}
