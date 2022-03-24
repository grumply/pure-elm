{ mkDerivation, base, containers, stm, pure, pure-lifted, pure-router, pure-txt, pure-uri, hashable, reflection, stdenv }:
mkDerivation {
  pname = "pure-elm";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure pure-lifted pure-router pure-txt pure-uri containers stm hashable reflection ];
  homepage = "github.com/grumply/pure-elm";
  description = "An implementation of the Elm architecture in Pure";
  license = stdenv.lib.licenses.bsd3;
}
