{
  stdenvNoCC,
  fetchFromGitHub,
  lib,
  ...
}:
stdenvNoCC.mkDerivation rec {
  pname = "plymouth-omarchy-theme";
  version = "v3.0.0";
  src = fetchFromGitHub {
    owner = "basecamp";
    repo = "omarchy";
    tag = "v3.0.0";
    hash = "sha256-dODPi1wWEK6uKzsahqza4TSwDGfzXpz0Kd8a5IRB7zI=";
  };
  sourceRoot = "${src.name}/default/plymouth";

  dontBuild = true;
  dontConfigure = true;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/plymouth/themes/omarchy
    cp * $out/share/plymouth/themes/omarchy
    find $out/share/plymouth/themes/ -name \*.plymouth -exec sed -i "s@\/usr\/@$out\/@" {} \;
    runHook postInstall
  '';
  meta = {
    description = "Omarchy splash screen.";
    homepage = "https://omarchy.org/";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
