unit uClass;

interface

uses
  uTypes, Trollhunter.Item.Types, uCreature, uSkill;

type
  TClassEnum = (clWarrior, clMage, clRanger, clThief);

const
  ClassMainWeapon: array [TClassEnum] of TItemEnum = (ivRusty_Sword, ivStaff1,
    ivBow1, ivDagger1);

type
  TClassSkillEnum = (skWeapon, skMain, skAdd);

type
  TClassProp = record
    Strength: TMinMax;
    Dexterity: TMinMax;
    Willpower: TMinMax;
    Perception: TMinMax;
    Life: TMinMax;
    Mana: TMinMax;
    Skill: array [TClassSkillEnum] of TSkillEnum;
  end;

const
  ClassProp: array [TClassEnum] of TClassProp = (
    // Warrior
    (Strength: (Min: 1; Max: 4;); Dexterity: (Min: 1; Max: 2;);
    Willpower: (Min: 0; Max: 0;); Perception: (Min: 0; Max: 0;);
    Life: (Min: 10; Max: 15;); Mana: (Min: 0; Max: 0;);
    Skill: (skBlade, skAthletics, skBodybuilding);),
    // Mage
    (Strength: (Min: 0; Max: 0;); Dexterity: (Min: 0; Max: 0;);
    Willpower: (Min: 1; Max: 4;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 0; Max: 0;); Mana: (Min: 15; Max: 25;);
    Skill: (skStaff, skConcentration, skMeditation);),
    // Ranger
    (Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 4;);
    Willpower: (Min: 0; Max: 0;); Perception: (Min: 0; Max: 0;);
    Life: (Min: 5; Max: 10;); Mana: (Min: 1; Max: 5;);
    Skill: (skBow, skDodge, skDodge);),
    // Thief
    (Strength: (Min: 0; Max: 0;); Dexterity: (Min: 1; Max: 2;);
    Willpower: (Min: 0; Max: 0;); Perception: (Min: 1; Max: 4;);
    Life: (Min: 5; Max: 7;); Mana: (Min: 5; Max: 7;);
    Skill: (skDagger, skToughness, skStealth);)
    /// ///
    );

type
  TClasses = class(TObject)
  private
    FSkills: TSkills;
    FClassName: array [TClassEnum] of string;
  private const
    FClassDescription: array [TClassEnum] of string = ('Warrior', 'Mage',
      'Ranger', 'Thief');
  public
    constructor Create;
    destructor Destroy; override;
    function GetName(I: TClassEnum): string;
    function GetDescription(I: TClassEnum): string;
    function GetSkills(I: TClassEnum): string;
    function GetItems(I: TClassEnum): string;
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
  FSkills := TSkills.Create;
end;

destructor TClasses.Destroy;
begin
  FreeAndNil(FSkills);
  inherited;
end;

function TClasses.GetDescription(I: TClassEnum): string;
begin
  Result := FClassDescription[I]
end;

function TClasses.GetItems(I: TClassEnum): string;
begin
  Result := '';
end;

function TClasses.GetName(I: TClassEnum): string;
begin
  Result := FClassName[I]
end;

function TClasses.GetSkills(I: TClassEnum): string;
var
  J: TClassSkillEnum;
  S: string;
  V: UInt;
begin
  Result := '';
  for J := Low(TClassSkillEnum) to High(TClassSkillEnum) do
  begin
    if (J <> High(TClassSkillEnum)) then
      V := BeginSkill
    else
      V := StartSkill;
    S := Format('%s (%d)', [FSkills.GetName(ClassProp[I].Skill[J]), V]);
    if (J <> High(TClassSkillEnum)) then
      Result := Result + S + ', '
    else
      Result := Result + S;
  end;

end;

initialization

Classes := TClasses.Create;

finalization

FreeAndNil(Classes);

end.
