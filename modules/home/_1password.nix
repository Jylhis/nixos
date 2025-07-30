{ lib, config, ... }:
{

  # TODO: Check also that 1password is installed
  programs.ssh.extraConfig = lib.optionalString config.programs.ssh.enable ''
    	IdentityAgent ~/.1password/agent.sock
        Include ~/.ssh/1Password/config
  '';
}
