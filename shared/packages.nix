# { nixpkgs ? import <nixpkgs> {}, compiler ? "802" }:
{ mkDerivation
, stdenv
, aeson
, time
, base64-bytestring
, bytestring
, hashable
, cereal
, binary
, persistent
, persistent-template
  }:
mkDerivation {
  pname = "server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  executableHaskellDepends = [
    aeson
    time
    base64-bytestring
    bytestring
    hashable
    cereal
    # - servant-auth-token-api
    binary
    persistent
    persistent-template
  ];
  license = stdenv.lib.licenses.bsd3;
}
