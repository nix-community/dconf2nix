# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
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
          dec = mkInt32 1234567890;
          dec-neg = mkInt32 (-987654321);
          dec-one = mkInt32 1;
          dec-pos = mkInt32 987654321;
          dec-zero = mkInt32 0;
          hex = mkInt32 1311768467463790320;
          hex-dec-only = mkInt32 78187493520;
          hex-neg = mkInt32 (-4660);
          hex-neg-e = mkInt32 (-30);
          hex-neg-e2 = mkInt32 (-482);
          hex-pos = mkInt32 4660;
          hex-zero = mkInt32 0;
          hex-zero-neg = mkInt32 0;
          hex-zero-pos = mkInt32 0;
          oct = mkInt32 2739128;
          oct-byte-max = mkInt32 255;
          oct-neg = mkInt32 (-255);
          oct-pos = mkInt32 255;
          oct-zero = mkInt32 0;
          oct-zero-neg = mkInt32 0;
          oct-zero-pos = mkInt32 0;
        };

      };
    }
  ];
}
