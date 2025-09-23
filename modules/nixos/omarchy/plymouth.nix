{ pkgs, ... }:
{
  # TODO: ../../../assets/system/plymouth/
  config = {
    boot.plymouth = {
      themePackages = [ pkgs.plymouth-omarchy-theme ];
      theme = "omarchy";
    };
  };
}
