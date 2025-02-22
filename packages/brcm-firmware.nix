{
  lib,
  stdenvNoCC,
  requireFile,
  name ? "bcrm-firmware.tar",
  hash ? lib.fakeHash,
}:

stdenvNoCC.mkDerivation (_: {
  name = "brcm-firmware";

  # Download the firmware script from: https://wiki.t2linux.org/tools/firmware.sh
  # Run the script and select option 2. Create a tarball of the firmware and extract it to Linux.
  # nix store prefetch-file --name firmware.tar --hash-type sha256 file:///home/$USER/path/to/firmware-mac-mini.tar
  src = requireFile {
    inherit name hash;
    url = "https://wiki.t2linux.org/tools/firmware.sh";

  };
  dontBuild = true;
  dontConfigure = true;
  dontInstall = true;
  dontPatch = true;

  unpackPhase = ''
    mkdir -p $out/lib/firmware/brcm
     tar -xf $src -C $out/lib/firmware/brcm
  '';
})
