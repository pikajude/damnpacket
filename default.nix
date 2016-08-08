{ mkDerivation, attoparsec, base, bytestring, fail, hspec
, html-entities, HUnit, QuickCheck, stdenv, template-haskell, text
, th-lift-instances
}:
mkDerivation {
  pname = "damnpacket";
  version = "1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring fail html-entities template-haskell text
    th-lift-instances
  ];
  testHaskellDepends = [ base bytestring hspec HUnit QuickCheck ];
  description = "Parsing dAmn messages";
  license = stdenv.lib.licenses.mit;
}
