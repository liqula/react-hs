{ mkDerivation, aeson, base, bytestring, ghcjs-base, mtl, stdenv
, string-conversions, template-haskell, text, time
, unordered-containers
}:
mkDerivation {
  pname = "react-hs";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring ghcjs-base mtl string-conversions
    template-haskell text time unordered-containers
  ];
  homepage = "https://github.com/liqula/react-hs";
  description = "A binding to React based on the Flux application architecture for GHCJS";
  license = stdenv.lib.licenses.bsd3;
}
