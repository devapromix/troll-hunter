unit uRace;

interface

uses
  Trollhunter.Types, uCreature;

type
  TRaceEnum = (rcHuman, rcElf, rcGnome, rcDwarf);

type
  TRaceProp = record
    Age: TMinMax;
    Height: TMinMax;
    Weight: TMinMax;
    Metabolism: TMinMax;
    Strength: TMinMax;
    Dexterity: TMinMax;
    Willpower: TMinMax;
    Perception: TMinMax;
    Life: TMinMax;
    Mana: TMinMax;
  end;

const
  RaceProp: array [TRaceEnum] of TRaceProp = (
    // Human
    (Age: (Min: 18; Max: 50;); Height: (Min: 160; Max: 180;);
    Weight: (Min: 70; Max: 110;); Metabolism: (Min: 80; Max: 85;);
    Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 2;);
    Willpower: (Min: 1; Max: 2;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 5; Max: 8;); Mana: (Min: 5; Max: 8;);),
    // Elf
    (Age: (Min: 75; Max: 800;); Height: (Min: 190; Max: 250;);
    Weight: (Min: 50; Max: 100;); Metabolism: (Min: 60; Max: 65;);
    Strength: (Min: 1; Max: 1;); Dexterity: (Min: 1; Max: 3;);
    Willpower: (Min: 1; Max: 3;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 5; Max: 10;); Mana: (Min: 10; Max: 15;);),
    // Gnome
    (Age: (Min: 20; Max: 160;); Height: (Min: 90; Max: 130;);
    Weight: (Min: 70; Max: 110;); Metabolism: (Min: 95; Max: 100;);
    Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 2;);
    Willpower: (Min: 1; Max: 3;); Perception: (Min: 1; Max: 3;);
    Life: (Min: 5; Max: 10;); Mana: (Min: 10; Max: 15;);),
    // Dwarf
    (Age: (Min: 20; Max: 250;); Height: (Min: 100; Max: 150;);
    Weight: (Min: 80; Max: 120;); Metabolism: (Min: 115; Max: 120;);
    Strength: (Min: 1; Max: 3;); Dexterity: (Min: 1; Max: 3;);
    Willpower: (Min: 1; Max: 1;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 10; Max: 15;); Mana: (Min: 5; Max: 10;);)
    /// ///
    );

type
  TRace = record

  end;

type
  TRaces = class(TObject)
  private
    FRaceName: array [TRaceEnum] of string;
  private const
    FRaceDescription: array [TRaceEnum] of string =
      ('Humans are the most common of races.', 'Elf', 'Gnome', 'Dwarf');
  public
    constructor Create;
    destructor Destroy; override;
    function GetName(I: TRaceEnum): string;
    function GetDescription(I: TRaceEnum): string;
  end;

var
  Races: TRaces;

implementation

uses TypInfo, SysUtils, uHelpers;

{ TRaces }

constructor TRaces.Create;
var
  I: TRaceEnum;
  P: Pointer;
begin
  P := TypeInfo(TRaceEnum);
  for I := Low(TRaceEnum) to High(TRaceEnum) do
    FRaceName[I] := GetEnumName(P, Ord(I)).GetName('rc');
end;

destructor TRaces.Destroy;
begin

  inherited;
end;

function TRaces.GetDescription(I: TRaceEnum): string;
begin
  Result := FRaceDescription[I]
end;

function TRaces.GetName(I: TRaceEnum): string;
begin
  Result := FRaceName[I]
end;

initialization

Races := TRaces.Create;

finalization

FreeAndNil(Races);

end.
