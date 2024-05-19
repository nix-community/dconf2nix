# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "floats" = {
      avogadro = mkDouble "6.283185307179586";
      basic = mkDouble "1.2345678901234567e9";
      negative = mkDouble "-72.02";
      positive = mkDouble "72.02";
      sci = mkDouble "6.022e23";
      sci-exp-neg = mkDouble "7.51e-9";
      sci-neg = mkDouble "-72020.0";
      sci-neg-exp-neg = mkDouble "-9.11e-17";
      zero = mkDouble "0.0";
      zero-neg = mkDouble "-0.0";
      zero-pos = mkDouble "0.0";
    };

    "int" = {
      dec = 1234567890;
      dec-neg = -987654321;
      dec-one = 1;
      dec-pos = 987654321;
      dec-zero = 0;
      hex = 1311768467463790320;
      hex-dec-only = 78187493520;
      hex-neg = -4660;
      hex-neg-e = -30;
      hex-neg-e2 = -482;
      hex-pos = 4660;
      hex-zero = 0;
      hex-zero-neg = 0;
      hex-zero-pos = 0;
      oct = 2739128;
      oct-byte-max = 255;
      oct-neg = -255;
      oct-pos = 255;
      oct-zero = 0;
      oct-zero-neg = 0;
      oct-zero-pos = 0;
    };

  };
}
