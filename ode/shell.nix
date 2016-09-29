let 
  lts = "lts-6_7";
in 
  with import <nixpkgs> { };
  haskell.lib.buildStackProject {
     ghc = haskell.packages.lts-6_7.ghc;
     name = "myapp";
     buildInputs = 
       [ zlib glib git cabal-install
         openssh autoreconfHook stack ];
   }
