{
  self,
  pkgs,
  emacs-ide,
  ...
}:
{
  environment.systemPackages = [
    self.outputs.packages.x86_64-linux.emacs-ide
    pkgs.bat
  ] ++ self.outputs.packages.x86_64-linux.emacs-ide.buildInputs;

  services.emacs = {
    enable = true;
    package = self.outputs.packages.x86_64-linux.emacs-ide;
  };

}
