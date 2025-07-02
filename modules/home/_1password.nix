{
  programs.ssh.extraConfig = ''
    	IdentityAgent ~/.1password/agent.sock
        Include ~/.ssh/1Password/config
  '';
}
