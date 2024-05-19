# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.hm.gvariant;

{
  dconf.settings = {
    "dict" = {
      alarms = [ [
        (mkDictionaryEntry ["name" (mkVariant "")])
        (mkDictionaryEntry ["id" (mkVariant "39d68c6c283032ad5549c9bd5f5161c4")])
        (mkDictionaryEntry ["active" (mkVariant false)])
        (mkDictionaryEntry ["hour" (mkVariant 17)])
        (mkDictionaryEntry ["minute" (mkVariant 50)])
        (mkDictionaryEntry ["days" (mkVariant (mkArray "i" []))])
        (mkDictionaryEntry ["snooze_minutes" (mkVariant 10)])
        (mkDictionaryEntry ["ring_minutes" (mkVariant 5)])
      ] ];
      dict = [
        (mkDictionaryEntry [1 "one"])
        (mkDictionaryEntry [2 "two"])
        (mkDictionaryEntry [3 "three"])
      ];
      dict-entry = mkDictionaryEntry [1 "one"];
      empty-arr-dict = mkArray "{sv}" [];
      empty-dict = mkTyped "a{sv}" [
      ];
      entry-arr = [ (mkDictionaryEntry [1 "one"]) (mkDictionaryEntry [2 "two"]) (mkDictionaryEntry [3 "three"]) ];
      nested = [ [
        (mkDictionaryEntry ["org.gnome.Contacts.desktop" (mkVariant [
          (mkDictionaryEntry ["position" (mkVariant 0)])
        ])])
        (mkDictionaryEntry ["org.gnome.Maps.desktop" (mkVariant [
          (mkDictionaryEntry ["position" (mkVariant 1)])
        ])])
        (mkDictionaryEntry ["org.gnome.Calculator.desktop" (mkVariant [
          (mkDictionaryEntry ["position" (mkVariant 2)])
        ])])
      ] ];
      timers = [ [
        (mkDictionaryEntry ["duration" (mkVariant 300)])
        (mkDictionaryEntry ["name" (mkVariant "")])
      ] ];
    };

  };
}
