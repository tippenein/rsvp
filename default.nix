{ reflex-platform ? import ./reflex-platform {}
, pkgs            ? import <nixpkgs>         {}
, ghc             ? pkgs.haskell.packages.ghc802
, ghcjs           ? reflex-platform.ghcjs
}:

let
  backend = ghc.callPackage    ./backend { inherit ghc;   };
  frontend = ghcjs.callPackage ./frontend { inherit ghcjs; };
in
  pkgs.writeScriptBin "serve" ''
    #!${pkgs.stdenv.shell}
    ${backend}/bin/backend ${frontend}/bin/frontend.jsexe/
  ''
