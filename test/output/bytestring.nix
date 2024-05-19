# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "bytestring" = {
      basic-double = mkByteString ''foo'';
      basic-single = mkByteString ''bar'';
      empty-double = mkByteString '''';
      empty-single = mkByteString '''';
      escapes-double = mkByteString ''\\\a\b\f\n\r\t\vcde"${"'"}${"'"}'';
      escapes-single = mkByteString ''\\\a\b\f\n\r\t\vcde""${"'"}'';
      line-continues-double = mkByteString ''start more'';
      line-continues-single = mkByteString ''start more'';
      nix-dollar = mkByteString ''${"$"}'';
      no-unicode = mkByteString ''u202F'';
      octal = mkByteString ''\3777\1\28\33${"$"}9m8"~\177'';
    };

  };
}
