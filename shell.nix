{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, errors, hpack, http-client-tls
      , servant, servant-client, stdenv, text, vector
      }:
      mkDerivation {
        pname = "repology-api";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [
          aeson base errors http-client-tls servant servant-client text
          vector
        ];
        preConfigure = "hpack";
        homepage = "https://github.com/ryantm/repology-api#readme";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
