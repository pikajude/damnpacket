{ mkDerivation, attoparsec, base, bytestring, fail, hspec
, html-entities, HUnit, QuickCheck, stdenv, text
}:
mkDerivation {
  pname = "damnpacket";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring fail html-entities text
  ];
  testHaskellDepends = [ base bytestring hspec HUnit QuickCheck ];
  license = stdenv.lib.licenses.mit;
}
