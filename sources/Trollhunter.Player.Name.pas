unit Trollhunter.Player.Name;

interface

uses
  Trollhunter.Player.Races,
  Trollhunter.Player.Types;

function GetRandomPlayerName(): string;

implementation

uses
  SysUtils,
  Trollhunter.Player,
  Trollhunter.UI.Log;

function GetRandomPlayerName(): string;
const
  // Human
  HumanPrefix: array[0..24] of string = (
    'Al','Ar','Bal','Bel','Bor','Cal','Cor','Dar','Dor','El',
    'Gar','Hal','Jar','Kel','Lor','Mal','Nar','Per','Rav','Tal',
    'Tor','Val','Var','Yor','Zar'
  );
  HumanMiddle: array[0..14] of string = (
    'a','e','i','o','u','an','ar','en','er','in','on','or','el','ir','ul'
  );
  HumanMaleSuffix: array[0..19] of string = (
    'd','n','r','ric','dor','mar','nar','rin','lan','gar',
    'ian','ion','ald','orn','vik','tur','rak','mir','kan','wyn'
  );
  HumanFemaleSuffix: array[0..19] of string = (
    'a','ia','elle','ina','ira','lyn','ria','ssa','tha','wen',
    'ara','essa','ora','eth','lia','mira','nna','ris','yra','una'
  );
  HumanMaleEnding: array[0..9] of string = (
    '','','n','r','s','or','ar','on','us','as'
  );
  HumanFemaleEnding: array[0..9] of string = (
    '','','a','ia','ra','na','la','elle','ine','ara'
  );

  // Elf
  ElfPrefix: array[0..24] of string = (
    'Ae','Ara','Cele','Elen','Fae','Gal','Ili','Lae','Loth','Mae',
    'Nim','Quel','Sil','Thal','Ther','Ael','Eir','Fing','Leg','Mir',
    'Ril','Syl','Tel','Vael','Yen'
  );
  ElfMiddle: array[0..14] of string = (
    'ae','ei','ia','ie','il','el','en','an','ar','or','ith','eth','ion','yr','wen'
  );
  ElfMaleSuffix: array[0..19] of string = (
    'ion','iel','las','dil','dir','rond','nor','mir','thal','dor',
    'wen','rin','las','orn','iel','ion','dil','thar','ven','yr'
  );
  ElfFemaleSuffix: array[0..19] of string = (
    'iel','wen','lith','riel','siel','eth','iel','ina','ara','elle',
    'yssa','thia','wen','riel','lith','essa','ora','via','yana','etha'
  );
  ElfMaleEnding: array[0..9] of string = (
    '','','ion','iel','las','dir','nor','mir','thal','orn'
  );
  ElfFemaleEnding: array[0..9] of string = (
    '','','iel','wen','lith','riel','eth','ara','elle','yssa'
  );

  // Gnome
  GnomePrefix: array[0..24] of string = (
    'Bim','Cob','Dim','Fiz','Gim','Hob','Jix','Kip','Lim','Nix',
    'Pim','Quib','Rix','Spro','Tib','Wib','Zim','Bok','Dab','Fen',
    'Glib','Klick','Nibble','Pip','Tock'
  );
  GnomeMiddle: array[0..9] of string = (
    'i','o','u','ee','oo','ix','ib','op','in','el'
  );
  GnomeMaleSuffix: array[0..19] of string = (
    'ble','kin','wick','wick','bert','nold','wick','gle','rick','wick',
    'bin','wig','nock','wick','bert','gle','rick','bin','wick','nub'
  );
  GnomeFemaleSuffix: array[0..19] of string = (
    'bie','nie','sie','mie','vie','kie','lie','pie','tie','wie',
    'ella','ina','ette','ie','y','bie','nie','sie','mie','vie'
  );
  GnomeMaleEnding: array[0..9] of string = (
    '','','kin','wick','bert','gle','rick','bin','nock','nub'
  );
  GnomeFemaleEnding: array[0..9] of string = (
    '','','bie','nie','sie','ella','ina','ette','ie','y'
  );

  // Dwarf
  DwarfPrefix: array[0..24] of string = (
    'Bal','Bor','Dur','Thr','Grim','Thor','Kar','Bro','Durin','Gim',
    'Khaz','Nor','Thrain','Ul','Vor','Bar','Dain','Grom','Krag','Mor',
    'Rur','Skor','Thok','Urist','Zog'
  );
  DwarfMiddle: array[0..9] of string = (
    'a','o','u','ar','or','ur','in','ak','ok','uk'
  );
  DwarfMaleSuffix: array[0..19] of string = (
    'in','or','un','ak','ok','grim','dor','gar','gor','kan',
    'kor','nar','nir','rok','thor','tur','var','vik','zor','dur'
  );
  DwarfFemaleSuffix: array[0..19] of string = (
    'a','ina','ora','una','dis','hild','run','thra','bera','dora',
    'gund','hild','ilda','kasa','mila','nara','olda','risa','saga','tora'
  );
  DwarfMaleEnding: array[0..9] of string = (
    '','','in','or','un','ak','ok','grim','dor','gar'
  );
  DwarfFemaleEnding: array[0..9] of string = (
    '','','a','ina','ora','dis','hild','run','bera','ilda'
  );

var
  S: string;
begin
  case Player.HRace of
    rcHuman:
      begin
        S := HumanPrefix[Random(Length(HumanPrefix))];
        if Random(3) = 0 then
          S := S + HumanMiddle[Random(Length(HumanMiddle))];
        if Player.Gender = gdMale then
          S := S + HumanMaleSuffix[Random(Length(HumanMaleSuffix))] +
               HumanMaleEnding[Random(Length(HumanMaleEnding))]
        else
          S := S + HumanFemaleSuffix[Random(Length(HumanFemaleSuffix))] +
               HumanFemaleEnding[Random(Length(HumanFemaleEnding))];
      end;
    rcElf:
      begin
        S := ElfPrefix[Random(Length(ElfPrefix))];
        if Random(2) = 0 then
          S := S + ElfMiddle[Random(Length(ElfMiddle))];
        if Player.Gender = gdMale then
          S := S + ElfMaleSuffix[Random(Length(ElfMaleSuffix))] +
               ElfMaleEnding[Random(Length(ElfMaleEnding))]
        else
          S := S + ElfFemaleSuffix[Random(Length(ElfFemaleSuffix))] +
               ElfFemaleEnding[Random(Length(ElfFemaleEnding))];
      end;
    rcGnome:
      begin
        S := GnomePrefix[Random(Length(GnomePrefix))];
        if Random(4) = 0 then
          S := S + GnomeMiddle[Random(Length(GnomeMiddle))];
        if Player.Gender = gdMale then
          S := S + GnomeMaleSuffix[Random(Length(GnomeMaleSuffix))] +
               GnomeMaleEnding[Random(Length(GnomeMaleEnding))]
        else
          S := S + GnomeFemaleSuffix[Random(Length(GnomeFemaleSuffix))] +
               GnomeFemaleEnding[Random(Length(GnomeFemaleEnding))];
      end;
    rcDwarf:
      begin
        S := DwarfPrefix[Random(Length(DwarfPrefix))];
        if Random(3) = 0 then
          S := S + DwarfMiddle[Random(Length(DwarfMiddle))];
        if Player.Gender = gdMale then
          S := S + DwarfMaleSuffix[Random(Length(DwarfMaleSuffix))] +
               DwarfMaleEnding[Random(Length(DwarfMaleEnding))]
        else
          S := S + DwarfFemaleSuffix[Random(Length(DwarfFemaleSuffix))] +
               DwarfFemaleEnding[Random(Length(DwarfFemaleEnding))];
      end;
  else
    S := 'Nameless';
  end;

  if Length(S) > 0 then
    S[1] := UpCase(S[1]);

  Result := S;
end;

end.
