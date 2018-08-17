unit Trollhunter.Player.Classes;

interface

uses
  Trollhunter.Types,
  Trollhunter.Item.Types,
  Trollhunter.Player.Types,
  Trollhunter.Player.Skills,
  Trollhunter.Creature,
  Trollhunter.Attribute;

type
  TClassEnum = (clWarrior, clRanger, clMonk, clMage, clThief);

type
  TClassSkillEnum = (skWeapon, skMain, skAdd);

type
  TClassArray<T> = array [TClassEnum] of T;

type
  TClassProp = record
    Description: string;
    Strength: TMinMax;
    Dexterity: TMinMax;
    Willpower: TMinMax;
    Perception: TMinMax;
    Life: TMinMax;
    Mana: TMinMax;
    Skill: array [TClassSkillEnum] of TSkillEnum;
    Item: array [stHead .. stFinger] of TItemEnum;
  end;

const
  ClassProp: TClassArray<TClassProp> = (
    // Warrior
    (Description: 'Warrior'; Strength: (Min: 1; Max: 4;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 0; Max: 0;); Perception: (Min: 0; Max: 0;);
    Life: (Min: 10; Max: 15;); Mana: (Min: 0; Max: 0;); Skill: (skBlade, skAthletics, skBodybuilding);
    Item: (ivCap, ivQuilted_Armor, None, None, ivRusty_Sword, ivBuckler, None, None);),
    // Ranger
    (Description: 'Ranger'; Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 4;); Willpower: (Min: 0; Max: 0;); Perception: (Min: 0; Max: 0;);
    Life: (Min: 5; Max: 10;); Mana: (Min: 1; Max: 5;); Skill: (skBow, skDodge, skDodge);
    Item: (ivCap, ivQuilted_Armor, None, None, ivShort_Bow, None, None, None);),
    // Monk
    (Description: 'Monk'; Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 2; Max: 3;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 3; Max: 7;); Mana: (Min: 11; Max: 14;); Skill: (skStaff, skConcentration, skMeditation);
    Item: (ivHood, ivLight_Clothes, None, None, ivQuarterstaff, None, None, None);),
    // Mage
    (Description: 'Mage'; Strength: (Min: 0; Max: 0;); Dexterity: (Min: 0; Max: 0;); Willpower: (Min: 1; Max: 4;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 0; Max: 0;); Mana: (Min: 15; Max: 25;); Skill: (skStaff, skConcentration, skMeditation);
    Item: (ivHood, ivLight_Clothes, None, None, ivShort_Staff, None, None, None);),
    // Thief
    (Description: 'Thief'; Strength: (Min: 0; Max: 0;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 0; Max: 0;); Perception: (Min: 1; Max: 4;);
    Life: (Min: 5; Max: 7;); Mana: (Min: 5; Max: 7;); Skill: (skDagger, skToughness, skStealth);
    Item: (ivCap, ivQuilted_Armor, None, None, ivDagger, None, None, None);)
    /// ///
    );

type
  TClasses = class(TObject)
  private
    FSkills: TSkills;
    FClassName: TClassArray<string>;
  public
    Attrib: TBaseAttribArray<UInt>;
    constructor Create;
    destructor Destroy; override;
    function GetName(I: TClassEnum): string;
    function GetDescription(I: TClassEnum): string;
    function GetSkills(I: TClassEnum): string;
    function GetItems(I: TClassEnum): string;
    function GetSkillBeginValue(ClassSkillEnum: TClassSkillEnum): UInt;
  end;

var
  PCClasses: TClasses;

implementation

uses
  SysUtils,
  Trollhunter.Helpers,
  Trollhunter.Utils,
  Trollhunter.Item,
  Trollhunter.Player,
  Trollhunter.UI,
  Trollhunter.Language,
  Trollhunter.Enum.Helpers;

{ TClasses }

constructor TClasses.Create;
var
  ClassEnum: TClassEnum;
begin
  for ClassEnum := Low(TClassEnum) to High(TClassEnum) do
    FClassName[ClassEnum] := Enum<TClassEnum>.ValueName(ClassEnum).GetName('cl');
  FSkills := TSkills.Create;
end;

destructor TClasses.Destroy;
begin
  FreeAndNil(FSkills);
  inherited;
end;

function TClasses.GetDescription(I: TClassEnum): string;
begin
  Result := ClassProp[I].Description;
end;

function TClasses.GetItems(I: TClassEnum): string;
var
  J: TSlotType;
  F: Boolean;
  ItemName: string;
begin
  F := False;
  Result := '';
  for J := Low(ClassProp[I].Item) to High(ClassProp[I].Item) do
    if (ClassProp[I].Item[J] <> TItemEnum.None) then
    begin
      ItemName := Items.Name[ClassProp[I].Item[J]];
      Utils.AppStr(Result, _(ItemName), F);
      F := True;
    end;
end;

function TClasses.GetName(I: TClassEnum): string;
begin
  Result := FClassName[I]
end;

function TClasses.GetSkillBeginValue(ClassSkillEnum: TClassSkillEnum): UInt;
begin
  case ClassSkillEnum of
    skWeapon, skMain:
      Result := BeginSkill;
  else
    Result := StartSkill;
  end;
end;

function TClasses.GetSkills(I: TClassEnum): string;
var
  J: TClassSkillEnum;
  F: Boolean;
  S: string;
begin
  F := False;
  Result := '';
  for J := Low(TClassSkillEnum) to High(TClassSkillEnum) do
  begin
    S := Format('%s %s%d', [_(FSkills.GetName(ClassProp[I].Skill[J])), UI.Icon(icPlus), GetSkillBeginValue(J)]);
    Utils.AppStr(Result, S, F);
    F := True;
  end;
end;

initialization

PCClasses := TClasses.Create;

finalization

FreeAndNil(PCClasses);

end.
