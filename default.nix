{ mkDerivation, base, containers, stm, pure, pure-router, pure-txt, hashable, stdenv }:
mkDerivation {
  pname = "pure-elm";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-router pure-txt containers stm hashable ];
  homepage = "github.com/grumply/pure-elm";
  description = "An implementation of the Elm architecture in Pure";
  license = stdenv.lib.licenses.bsd3;
}
