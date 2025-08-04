{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.programs.aider;
  format = pkgs.formats.yaml { };
  freeformType = (pkgs.formats.yaml { }).type;

in
{
  options.programs.aider = {
    enable = lib.mkEnableOption "aider";
    package = lib.mkPackageOption pkgs "aider-chat-full" { };
    settings = lib.mkOption {
      description = "https://aider.chat/docs/config/aider_conf.html";
      default = { };
      type = lib.types.submodule {
        inherit freeformType;
        options = {
          verify-ssl = lib.mkOption {
            type = lib.types.bool;
            default = true;
            description = "Verify the SSL cert when connecting to models";
          };
          architect = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Use architect edit format for the main chat";
          };
          model = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Specify the model to use for the main chat";
          };
          weak-model = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Specify the model to use for commit messages and chat history summarization";
          };
          editor-model = lib.mkOption {
            type = lib.types.nullOr lib.types.str;
            default = null;
            description = "Specify the model to use for editor tasks (default depends on --model)";
          };
          set-env = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Set an environment variable";
          };
          alias = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            example = [ "fast:openai/o3-mini" ];
            description = "Add a model alias";
          };

          # CACHE
          cache-prompts = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable caching of prompts";
          };

          # OUTPUT
          dark-mode = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Use colors suitable for a dark terminal background";
          };
          light-mode = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Use colors suitable for a light terminal background";
          };

          # Analytics
          analytics = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = "Enable/disable analytics for current session";
          };
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    home = {
      packages = [ cfg.package ];
      file.".aider.conf.yml".source = format.generate ".aider.conf.yml" (
        lib.filterAttrs (
          _name: value: value != null && (if builtins.isList value then value != [ ] else true)
        ) cfg.settings
      );
    };
  };
}
