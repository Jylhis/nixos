pkgs: {
  dconf-dump = {
    type = "app";
    program = pkgs.lib.getExe (
      pkgs.writeShellApplication {
        name = "dconf-dump";
        runtimeInputs = [
          pkgs.dconf2nix
          pkgs.gnused
          pkgs.nixfmt-rfc-style
        ];
        bashOptions = [
          "errexit"
          "pipefail"
        ];
        # sed command is due to this: https://github.com/nix-community/dconf2nix/issues/112
        text = ''
                            if [ -z "$1" ]; then
                                echo "Usage: nix run .#dconf-dump path/to/output-file.nix"
                                exit 1
                            fi

                            dconf dump / \
                            | sed "/mru-sources/d" \
                            | dconf2nix \
          		  | nixfmt > "$1"
        '';
      }
    );
  };
}
