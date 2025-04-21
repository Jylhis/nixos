{
  config,
  ...
}:
{
  config = {
    users.users.markus = {
      isNormalUser = true;
      description = "Markus";

      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK4zXcaYT+RTxvAjUjE3B33kwwxCOo4ApI4diLnajbUT"
      ];
    };
  };

}
