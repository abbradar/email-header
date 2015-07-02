{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, containers, exceptions, QuickCheck, stdenv
, tasty, tasty-quickcheck, text, text-icu, time
}:
mkDerivation {
  pname = "email-header";
  version = "0.3.0";
  src = ./.;
  buildDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    containers exceptions text text-icu time
  ];
  testDepends = [
    base bytestring case-insensitive containers QuickCheck tasty
    tasty-quickcheck text time
  ];
  homepage = "http://github.com/knrafto/email-header";
  description = "Parsing and rendering of email and MIME headers";
  license = stdenv.lib.licenses.bsd3;
}
