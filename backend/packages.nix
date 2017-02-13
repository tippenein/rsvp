{ mkDerivation
, base
, shared
, stdenv
, protolude
, aeson
, bytestring
, esqueleto
, exceptions
, http-types
, logging-effect
, monad-logger
, mtl
, optparse-applicative
, prometheus-client
, prometheus-metrics-ghc
, persistent
, persistent-postgresql
, persistent-template
, servant
, servant-server
, servant-auth-token
, servant-auth-token-api
, text
, time
, transformers
, wai
, wai-extra
, wai-cors
, wai-middleware-prometheus
, warp
, wl-pprint-text
, http-media
, neat-interpolation
}:

mkDerivation {
  pname = "backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  # executableHaskellDepends = [
  buildDepends = [
    base
    shared
    protolude
    aeson
    bytestring
    esqueleto
    exceptions
    http-media
    http-types
    logging-effect
    monad-logger
    mtl
    optparse-applicative
    prometheus-client
    prometheus-metrics-ghc
    persistent
    persistent-postgresql
    persistent-template
    servant
    servant-server
    servant-auth-token
    servant-auth-token-api
    text
    time
    transformers
    wai
    wai-extra
    wai-cors
    wai-middleware-prometheus
    warp
    wl-pprint-text
    neat-interpolation
  ];
  license = stdenv.lib.licenses.bsd3;
}
