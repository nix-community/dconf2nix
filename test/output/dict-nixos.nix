# Generated via dconf2nix: https://github.com/nix-commmunity/dconf2nix
{ lib, ... }:

with lib.gvariant;

{
  programs.dconf.profiles.user.databases = [
    {
      settings = {
        "dict" = {
          alarms = [ [
            (mkDictionaryEntry "name" (mkVariant ""))
            (mkDictionaryEntry "id" (mkVariant "39d68c6c283032ad5549c9bd5f5161c4"))
            (mkDictionaryEntry "active" (mkVariant false))
            (mkDictionaryEntry "hour" (mkVariant (mkInt32 17)))
            (mkDictionaryEntry "minute" (mkVariant (mkInt32 50)))
            (mkDictionaryEntry "days" (mkVariant (mkArray "i" [])))
            (mkDictionaryEntry "snooze_minutes" (mkVariant (mkInt32 10)))
            (mkDictionaryEntry "ring_minutes" (mkVariant (mkInt32 5)))
          ] ];
          dict = [
            (mkDictionaryEntry (mkInt32 1) "one")
            (mkDictionaryEntry (mkInt32 2) "two")
            (mkDictionaryEntry (mkInt32 3) "three")
          ];
          dict-entry = mkDictionaryEntry (mkInt32 1) "one";
          empty-arr-dict = mkArray "{sv}" [];
          empty-dict = mkTyped "a{sv}" [
          ];
          entry-arr = [ (mkDictionaryEntry (mkInt32 1) "one") (mkDictionaryEntry (mkInt32 2) "two") (mkDictionaryEntry (mkInt32 3) "three") ];
          nested = [ [
            (mkDictionaryEntry "org.gnome.Contacts.desktop" (mkVariant [
              (mkDictionaryEntry "position" (mkVariant (mkInt32 0)))
            ]))
            (mkDictionaryEntry "org.gnome.Maps.desktop" (mkVariant [
              (mkDictionaryEntry "position" (mkVariant (mkInt32 1)))
            ]))
            (mkDictionaryEntry "org.gnome.Calculator.desktop" (mkVariant [
              (mkDictionaryEntry "position" (mkVariant (mkInt32 2)))
            ]))
          ] ];
          timers = [ [
            (mkDictionaryEntry "duration" (mkVariant (mkInt32 300)))
            (mkDictionaryEntry "name" (mkVariant ""))
          ] ];
        };

      };
    }
  ];
}
