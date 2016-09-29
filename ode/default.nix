 with import <nixpkgs> {}; {
  odeEnv = stdenv.mkDerivation {
    name = "odeEnv";
    buildInputs = [
      zlib.out
      zlib.dev
      cairo
      pango
      gnome.gtk
      pkgconfig
    ];
  };
}
