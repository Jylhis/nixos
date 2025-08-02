{ pkgs, ... }:
{

  home = {
    packages = with pkgs; [
      aider-chat-full
    ];
  };
  #services.ollama.enable = true;
}
