unit uRace;

interface

uses
  uTypes, uCreature;

type
  TRaceEnum = (rcHuman, rcElf, rcDwarf);

const
  RaceAge: array[TRaceEnum] of TMinMax = (
  // Human
  (Min: 18; Max: 50;),
  // Elf
  (Min: 25; Max: 500;),
  // Dwarf
  (Min: 20; Max: 200;)
  );

const
  RaceHeight: array[TRaceEnum] of TMinMax = (
  // Human
  (Min: 160; Max: 180;),
  // Elf
  (Min: 180; Max: 220;),
  // Dwarf
  (Min: 100; Max: 130;)
  );

const
  RaceWeight: array[TRaceEnum] of TMinMax = (
  // Human
  (Min: 70; Max: 110;),
  // Elf
  (Min: 50; Max: 110;),
  // Dwarf
  (Min: 60; Max: 110;)
  );

type
  TRace = record

  end;

type
  TRaces = class(TObject)
  private
    FRaceName: array [TRaceEnum] of string;
    FRace: array [TRaceEnum] of TRace;
    function GetRace(I: TRaceEnum): TRace;
    procedure SetRace(I: TRaceEnum; const Value: TRace);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Race[I: TRaceEnum]: TRace read GetRace write SetRace;
    function GetName(I: TRaceEnum): string;
  end;

var
  Races: TRaces;

implementation

uses TypInfo, SysUtils, uHelpers;

{ TRaces }

procedure TRaces.Clear;
var
  I: TRaceEnum;
begin
  for I := Low(TRaceEnum) to High(TRaceEnum) do
    with FRace[I] do
    begin

    end;
end;

constructor TRaces.Create;
var
  I: TRaceEnum;
  P: Pointer;
begin
  Self.Clear;
  P := TypeInfo(TRaceEnum);
  for I := Low(TRaceEnum) to High(TRaceEnum) do
    FRaceName[I] := GetEnumName(P, Ord(I)).GetName('rc');
end;

destructor TRaces.Destroy;
begin

  inherited;
end;

function TRaces.GetName(I: TRaceEnum): string;
begin
  Result := FRaceName[I]
end;

function TRaces.GetRace(I: TRaceEnum): TRace;
begin
  Result := FRace[I]
end;

procedure TRaces.SetRace(I: TRaceEnum; const Value: TRace);
begin
  FRace[I] := Value
end;

initialization

Races := TRaces.Create;

finalization

FreeAndNil(Races);

end.
