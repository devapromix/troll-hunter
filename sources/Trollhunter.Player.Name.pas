unit Trollhunter.Player.Name;

interface

uses
  Trollhunter.Player.Races,
  Trollhunter.Player.Types;

function GetRandomPlayerName(): string;

implementation

uses
  SysUtils,
  Trollhunter.Player;

function GetRandomPlayerName(): string;
const
  // Human
  HumanPrefix: array[0..20] of string = (
    'Al', 'Ar', 'Bal', 'Bel', 'Bor', 'Cal', 'Cor', 'Dan', 'Dar', 'Dor', 'El',
    'Gar', 'Hal', 'Jar', 'Kel', 'Lor', 'Mal', 'Nar', 'Tal', 'Val', 'Zar'
  );
  HumanMaleSuffix: array[0..14] of string = (
    'ric', 'dor', 'mar', 'nar', 'rin', 'lan', 'gar', 'ian', 'ion',
    'ald', 'orn', 'vik', 'tur', 'mir', 'kan'
  );
  HumanFemaleSuffix: array[0..14] of string = (
    'ia', 'elle', 'ina', 'ira', 'lyn', 'ria', 'ssa', 'tha', 'wen',
    'ara', 'essa', 'ora', 'eth', 'lia', 'mira'
  );

  // Elf
  ElfPrefix: array[0..19] of string = (
    'Ae', 'Ara', 'Cele', 'Elen', 'Fae', 'Gal', 'Ili', 'Lae', 'Loth', 'Mae',
    'Nim', 'Quel', 'Sil', 'Thal', 'Ther', 'Ael', 'Eir', 'Leg', 'Mir', 'Syl'
  );
  ElfMaleSuffix: array[0..14] of string = (
    'ion', 'iel', 'las', 'dil', 'dir', 'rond', 'nor', 'mir', 'thal',
    'dor', 'wen', 'rin', 'orn', 'thar', 'ven'
  );
  ElfFemaleSuffix: array[0..14] of string = (
    'iel', 'wen', 'lith', 'riel', 'siel', 'eth', 'ina', 'ara', 'elle',
    'yssa', 'thia', 'essa', 'ora', 'via', 'yana'
  );

  // Gnome
  GnomePrefix: array[0..19] of string = (
    'Bim', 'Cob', 'Dim', 'Fiz', 'Gim', 'Hob', 'Jix', 'Kip', 'Lim', 'Nix',
    'Pim', 'Quib', 'Rix', 'Spro', 'Tib', 'Wib', 'Zim', 'Bok', 'Fen', 'Pip'
  );
  GnomeMaleSuffix: array[0..14] of string = (
    'ble', 'kin', 'wick', 'bert', 'nol', 'gle', 'rick', 'bin',
    'wig', 'nock', 'nub', 'tock', 'zap', 'fix', 'nix'
  );
  GnomeFemaleSuffix: array[0..14] of string = (
    'bie', 'nie', 'sie', 'mie', 'vie', 'kie', 'lie', 'pie',
    'tie', 'wie', 'ella', 'ina', 'ette', 'ie', 'y'
  );

  // Dwarf
  DwarfPrefix: array[0..19] of string = (
    'Bal', 'Bor', 'Dur', 'Thr', 'Grim', 'Thor', 'Kar', 'Bro', 'Gim', 'Khaz',
    'Nor', 'Ul', 'Vor', 'Bar', 'Dain', 'Grom', 'Krag', 'Mor', 'Rur', 'Thok'
  );
  DwarfMaleSuffix: array[0..14] of string = (
    'in', 'or', 'un', 'ak', 'ok', 'rum', 'dor', 'gar', 'gor',
    'kan', 'kor', 'nar', 'nir', 'rok', 'dur'
  );
  DwarfFemaleSuffix: array[0..14] of string = (
    'a', 'ina', 'ora', 'una', 'dis', 'hild', 'run', 'thra',
    'bera', 'dora', 'gund', 'ilda', 'kasa', 'mila', 'nara'
  );

var
  Prefix, Suffix: string;
begin
  case Player.HRace of
    rcHuman:
      begin
        Prefix := HumanPrefix[Random(Length(HumanPrefix))];
        if Player.Gender = gdMale then
          Suffix := HumanMaleSuffix[Random(Length(HumanMaleSuffix))]
        else
        begin
          Suffix := HumanFemaleSuffix[Random(Length(HumanFemaleSuffix))];
        end;
      end;

    rcElf:
      begin
        Prefix := ElfPrefix[Random(Length(ElfPrefix))];
        if Player.Gender = gdMale then
          Suffix := ElfMaleSuffix[Random(Length(ElfMaleSuffix))]
        else
        begin
          Suffix := ElfFemaleSuffix[Random(Length(ElfFemaleSuffix))];
        end;
      end;

    rcGnome:
      begin
        Prefix := GnomePrefix[Random(Length(GnomePrefix))];
        if Player.Gender = gdMale then
          Suffix := GnomeMaleSuffix[Random(Length(GnomeMaleSuffix))]
        else
        begin
          Suffix := GnomeFemaleSuffix[Random(Length(GnomeFemaleSuffix))];
        end;
      end;

    rcDwarf:
      begin
        Prefix := DwarfPrefix[Random(Length(DwarfPrefix))];
        if Player.Gender = gdMale then
          Suffix := DwarfMaleSuffix[Random(Length(DwarfMaleSuffix))]
        else
        begin
          Suffix := DwarfFemaleSuffix[Random(Length(DwarfFemaleSuffix))];
        end;
      end;
  else
    begin
      Result := 'Nameless';
      Exit;
    end;
  end;

  Result := Prefix + Suffix;

  if Length(Result) > 0 then
    Result[1] := UpCase(Result[1]);
end;

end.
