{ mkDerivation, base, containers, stm, pure, stdenv }:
mkDerivation {
  pname = "pure-elm";
  version = "0.7.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure containers stm ];
  homepage = "github.com/grumply/pure-elm";
  description = "An implementation of the Elm architecture in Pure";
  license = stdenv.lib.licenses.bsd3;
}
