{ pkgs ? import <nixpkgs> {}
, ghc
, shared ? import ../shared
}:
let
  drv = ghc.callPackage ./packages.nix {
  shared = shared { compiler = ghc; };
};
in
  if pkgs.lib.inNixShell then drv.env else drv
