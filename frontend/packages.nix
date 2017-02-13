{ mkDerivation, reflex, reflex-dom, stdenv, shared
}:
mkDerivation {
  pname = "frontend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    reflex reflex-dom shared
  ];
  license = stdenv.lib.licenses.bsd3;
}
