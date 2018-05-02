unit uClass;

interface

uses
  uTypes, uCreature;

type
  TClassEnum = (clWarrior, clMage, clRanger, clThief);

type
  TClassProp = record
    Strength: TMinMax;
    Dexterity: TMinMax;
    Willpower: TMinMax;
    Perception: TMinMax;
    Life: TMinMax;
    Mana: TMinMax;
  end;

const
  ClassProp: array [TClassEnum] of TClassProp = (
    // Warrior
    (Strength: (Min: 1; Max: 4;); Dexterity: (Min: 1; Max: 2;);
    Willpower: (Min: 0; Max: 0;); Perception: (Min: 0; Max: 0;);
    Life: (Min: 10; Max: 15;); Mana: (Min: 0; Max: 0;);),
    // Mage
    (Strength: (Min: 0; Max: 0;); Dexterity: (Min: 0; Max: 0;);
    Willpower: (Min: 1; Max: 4;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 0; Max: 0;); Mana: (Min: 15; Max: 25;);),
    // Ranger
    (Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 4;);
    Willpower: (Min: 0; Max: 0;); Perception: (Min: 0; Max: 0;);
    Life: (Min: 5; Max: 10;); Mana: (Min: 1; Max: 5;);),
    // Thief
    (Strength: (Min: 0; Max: 0;); Dexterity: (Min: 1; Max: 2;);
    Willpower: (Min: 0; Max: 0;); Perception: (Min: 1; Max: 4;);
    Life: (Min: 5; Max: 7;); Mana: (Min: 5; Max: 7;);)
    /// ///
    );

type
  TClasses = class(TObject)
  private
    FClassName: array [TClassEnum] of string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetName(I: TClassEnum): string;
  end;

var
  Classes: TClasses;

implementation

uses SysUtils, TypInfo, uHelpers;

{ TClasses }

constructor TClasses.Create;
var
  I: TClassEnum;
  P: Pointer;
begin
  P := TypeInfo(TClassEnum);
  for I := Low(TClassEnum) to High(TClassEnum) do
    FClassName[I] := GetEnumName(P, Ord(I)).GetName('cl');
end;

destructor TClasses.Destroy;
begin

  inherited;
end;

function TClasses.GetName(I: TClassEnum): string;
begin
  Result := FClassName[I]
end;

initialization

Classes := TClasses.Create;

finalization

FreeAndNil(Classes);

end.
