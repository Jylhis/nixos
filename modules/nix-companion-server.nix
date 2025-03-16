{ config, pkgs, ... }:
{
  imports = [ ];
  options = {

  };
  config = {

    nix = {
      settings = {
        substituters = [
          "ssh://nix-ssh@lab?trusted=true"
        ];
        trusted-public-keys = [
          "jylhis.com:2tR5lVPX2yBu38X3n2rkKYcXJdjbCcuiCgh4byzA21I="
        ];
        trusted-substituters = [
          "ssh://nix-ssh@lab"
        ];
      };
      buildMachines = [
        {
          hostName = "lab";
          system = "x86_64-linux";
          sshUser = "nixremote";
          protocol = "ssh-ng";
          publicHostKey = "c3NoLWVkMjU1MTkgQUFBQUMzTnphQzFsWkRJMU5URTVBQUFBSUcxRmxmRW9lck1Ma1kvYTEvY0l4NTdkbGc2Z2JlcXBzeGJ6SEI4VjlYNksgcm9vdEBtYWNib29rLWFpcgo=";
          sshKey = "/etc/ssh/ssh_host_ed25519_key";
          maxJobs = 3;
          speedFactor = 4;
          supportedFeatures = [
            "nixos-test"
            "benchmark"
            "big-parallel"
            "kvm"
          ];
          mandatoryFeatures = [ ];
        }
      ];

      distributedBuilds = true;
      settings = {
        builders-use-substitutes = true;
      };
    };

    # known Long-lived public host keys
    programs.ssh = {
      extraConfig = ''
        	    Match Host lab User nix-ssh
                  IdentitiesOnly yes
                  IdentityFile /etc/ssh/ssh_host_ed25519_key
      '';
      knownHostsFiles = [

        (pkgs.writeText "lab.pub" ''
          lab ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL9H7DHiSGfKDJ2rhDsuuCIQZgTcbzd9cOln57E43o55
        '')
      ];
    };

  };
}
