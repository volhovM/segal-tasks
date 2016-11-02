with import <nixpkgs> { };

let 
  hsPkgs = haskell.packages.ghc801;
in 
  haskell.lib.buildStackProject {
     #ghc = hsPkgs.ghc;
     ghc = hsPkgs.ghcWithPackages (p: with p; [
       gtk2hs-buildtools # to build Chart-cairo (https://github.com/gtk2hs/gtk2hs/issues/179)
     ]);
     name = "fdm";
     buildInputs = [
       zlib
       cairo
       pango
       gnome2.gtk
     ];
   }
