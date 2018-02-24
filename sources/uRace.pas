unit uRace;

interface

uses
  uTypes, uCreature;

type
  TRaceEnum = (rcHuman, rcElf, rcDwarf);

type
  TRaceProp = record
    Age: TMinMax;
    Height: TMinMax;
    Weight: TMinMax;
  end;

const
  RaceProp: array[TRaceEnum] of TRaceProp = (
  // Human
  (Age: (Min: 18; Max: 50;); Height: (Min: 160; Max: 180;); Weight: (Min: 70; Max: 110;);),
  // Elf
  (Age: (Min: 25; Max: 500;); Height: (Min: 190; Max: 250;); Weight: (Min: 50; Max: 100;);),
  // Dwarf
  (Age: (Min: 20; Max: 200;); Height: (Min: 100; Max: 150;); Weight: (Min: 80; Max: 120;);)
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
