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
  # nix store add-file --name macbook-air-firmware.tar
  src = requireFile {
    inherit name hash;

    message = "nix store add-file --name macbook-air-firmware.tar.gz";

  };
  unpackPhase = ''
    mkdir -p $out/lib/firmware/brcm
    tar xvf $src --dir "$out/lib/firmware/brcm"
  '';
})
