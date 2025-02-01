{
  lib,
  stdenvNoCC,
  requireFile,
  name ? "bcrm-firmware.tar.gz",
  hash ? lib.fakeHash,
}:

stdenvNoCC.mkDerivation (_: {
  name = "brcm-firmware";
  src = requireFile {
    inherit name hash;

    message = "nix store add-file --name macbook-air-firmware.tar.gz";

  };
  unpackPhase = ''
    mkdir -p $out/lib/firmware/brcm
    tar xzf $src --dir "$out/lib/firmware/brcm"
  '';
})
