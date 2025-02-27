{
  lib,
  stdenv,
  fetchFromGitHub,
  gradle,
  jre,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "atsumeru";
  version = "1.1";

  src = fetchFromGitHub {
    owner = "Atsumeru-xyz";
    repo = "Atsumeru";
    tag = "1.1";
    hash = "sha256-tkPRDSvnjNo0ClNKlOUG/mZEMgOxuIWQ5D1Oui8O2j0=";
  };
  nativeBuildInputs = [ gradle ];

  mitmCache = gradle.fetchDeps {
    inherit (finalAttrs) pname;
    data = ./deps.json;
  };
  #  gradleBuildTask = "shadowJar";

  doCheck = false;

  installPhase = ''
    mkdir -p $out/{bin,share/atsumeru}
    cp build/libs/atsumeru-all.jar $out/share/atsumeru

    makeWrapper ${jre}/bin/java $out/bin/atsumeru \
      --add-flags "-jar $out/share/pdftk/atsumeru-all.jar"


  '';

  meta.sourceProvenance = with lib.sourceTypes; [
    fromSource
    binaryBytecode # mitm cache
  ];
})
