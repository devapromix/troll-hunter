unit Trollhunter.Player.Classes;

interface

uses
  Trollhunter.Types,
  Trollhunter.Item.Types,
  Trollhunter.Player.Types,
  Trollhunter.Ability,
  Trollhunter.Creature,
  Trollhunter.Skill;

type
  TClassEnum = (clWarrior, clMage, clRanger, clThief);

type
  TClassSkillEnum = (skWeapon, skMain, skAdd);

type
  TClassProp = record
    Description: string;
    Strength: TMinMax;
    Dexterity: TMinMax;
    Willpower: TMinMax;
    Perception: TMinMax;
    Life: TMinMax;
    Mana: TMinMax;
    Ability: TAbilityEnum;
    Skill: array [TClassSkillEnum] of TSkillEnum;
    EquipItem: array [stHead .. stSpellbook] of TItemEnum;
    ClassItem: array of TItemEnum;
  end;

const
  ClassProp: array [TClassEnum] of TClassProp = (
    // Warrior
    (Description: 'A strong warrior skilled in melee combat. Relies on power, heavy armor and steel to crush enemies.';
     Strength: (Min: 1; Max: 4; ); Dexterity: (Min: 1; Max: 2; );
     Willpower: (Min: 0; Max: 0; ); Perception: (Min: 0; Max: 0; );
     Life: (Min: 10; Max: 15; ); Mana: (Min: 0; Max: 0; ); Ability: abFind_Item;
     Skill: (skBlade, skAthletics, skBodybuilding);
     EquipItem: (itmCap, itmQuilted_Armor, itmNone, itmNone, itmRusty_Sword, itmBuckler,
     itmNone, itmNone, itmNone, itmNone, itmNone); ),

    // Mage
    (Description: 'A powerful mage who wields ancient magic. Weak in close combat but deadly with spells at range.';
     Strength: (Min: 0; Max: 0; ); Dexterity: (Min: 0; Max: 0; );
     Willpower: (Min: 1; Max: 4; ); Perception: (Min: 1; Max: 2; );
     Life: (Min: 0; Max: 0; ); Mana: (Min: 15; Max: 25; ); Ability: abConjure_Mana_Orb;
     Skill: (skStaff, skConcentration, skMeditation);
     EquipItem: (itmHood, itmLight_Clothes, itmNone, itmNone, itmQuarterstaff, itmNone,
     itmYew_Wand, itmNone, itmNone, itmSpellbook, itmNone);
     ClassItem: [itmElemental_Book_of_Fire_Arrow]),

    // Ranger
    (Description: 'A skilled hunter and archer. Agile and versatile, excels in forests and open terrain.';
     Strength: (Min: 0; Max: 0; ); Dexterity: (Min: 1; Max: 4; );
     Willpower: (Min: 0; Max: 0; ); Perception: (Min: 1; Max: 2; );
     Life: (Min: 5; Max: 10; ); Mana: (Min: 1; Max: 5; ); Ability: abCripling_Blow;
     Skill: (skBow, skDodge, skAwareness);
     EquipItem: (itmCap, itmQuilted_Armor, itmNone, itmNone, itmSmall_Dagger, itmNone,
     itmShort_Bow, itmLight_Quiver, itmNone, itmNone, itmNone); ),

    // Thief
    (Description: 'A nimble rogue and shadow master. Deadly in stealth, ambushes and quick strikes.';
     Strength: (Min: 0; Max: 0; ); Dexterity: (Min: 1; Max: 2; );
     Willpower: (Min: 0; Max: 0; ); Perception: (Min: 1; Max: 4; );
     Life: (Min: 5; Max: 7; ); Mana: (Min: 5; Max: 7; ); Ability: abStealth;
     Skill: (skDagger, skToughness, skStealth);
     EquipItem: (itmCap, itmQuilted_Armor, itmNone, itmNone, itmShort_Dagger, itmNone,
     itmNone, itmNone, itmNone, itmNone, itmNone);  ClassItem: [itmWyvern_Venom])
  );
type

  { TClasses }

  TClasses = class(TObject)
  private
    FSkills: TSkills;
    FClassName: array [TClassEnum] of string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetName(I: TClassEnum): string;
    function GetDescription(I: TClassEnum): string;
    function GetSkills(I: TClassEnum): string;
    function GetAbility(I: TClassEnum): string;
    function GetItems(I: TClassEnum): string;
    function GetSkillBeginValue(ClassSkillEnum: TClassSkillEnum): UInt;
  end;

var
  Classes: TClasses;

implementation

uses
  SysUtils,
  TypInfo,
  Trollhunter.Helpers,
  Trollhunter.Utils,
  Trollhunter.Item;

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
  Result := ClassProp[I].Description;
end;

function TClasses.GetItems(I: TClassEnum): string;
var
  J: TSlotType;
  F: boolean;
  Z: Integer;

  procedure Add(AName: string);
  begin
    if AName <> 'None' then
      Utils.AppStr(Result, AName, F);
  end;

begin
  F := False;
  Result := '';
  for J := Low(ClassProp[I].EquipItem) to High(ClassProp[I].EquipItem) do
    if (ClassProp[I].EquipItem[J] <> TItemEnum.itmNone) then
    begin
      Add(Items.Name[ClassProp[I].EquipItem[J]]);
      F := True;
    end;
  for Z := 0 to Length(ClassProp[I].ClassItem) -1 do
    Add(Items.Name[ClassProp[I].ClassItem[Z]]);
end;

function TClasses.GetName(I: TClassEnum): string;
begin
  Result := FClassName[I];
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
  F: boolean;
  S: string;
begin
  F := False;
  Result := '';
  for J := Low(TClassSkillEnum) to High(TClassSkillEnum) do
  begin
    S := Format('%s +%d', [FSkills.GetName(ClassProp[I].Skill[J]),
      GetSkillBeginValue(J)]);
    Utils.AppStr(Result, S, F);
    F := True;
  end;
end;

function TClasses.GetAbility(I: TClassEnum): string;
begin
  Result := AbilityBase[ClassProp[I].Ability].Name;
end;

initialization

  Classes := TClasses.Create;

finalization

  FreeAndNil(Classes);

end.
