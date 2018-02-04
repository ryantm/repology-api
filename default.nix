{ mkDerivation, aeson, base, hpack, servant, stdenv }:
mkDerivation {
  pname = "repology-api";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ aeson base servant ];
  preConfigure = "hpack";
  homepage = "https://github.com/ryantm/repology-api#readme";
  license = stdenv.lib.licenses.bsd3;
}
