unit Trollhunter.Player.Classes;

interface

uses
  Trollhunter.Types,
  Trollhunter.Item.Types,
  Trollhunter.Player.Types,
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
    Skill: array [TClassSkillEnum] of TSkillEnum;
    Item: array [stHead .. stFinger] of TItemEnum;
  end;

const
  ClassProp: array [TClassEnum] of TClassProp = (
    // Warrior
    (Description: 'Warrior'; Strength: (Min: 1; Max: 4; ); Dexterity: (Min: 1;
    Max: 2; ); Willpower: (Min: 0; Max: 0; ); Perception: (Min: 0; Max: 0; );
    Life: (Min: 10; Max: 15; ); Mana: (Min: 0; Max: 0; );
    Skill: (skBlade, skAthletics, skBodybuilding);
    Item: (itmCap, itmQuilted_Armor, itmNone, itmNone, itmRusty_Sword, itmBuckler,
    itmNone, itmNone, itmNone, itmNone); ),
    // Mage
    (Description: 'Mage'; Strength: (Min: 0; Max: 0; ); Dexterity: (Min: 0;
    Max: 0; ); Willpower: (Min: 1; Max: 4; ); Perception: (Min: 1; Max: 2; );
    Life: (Min: 0; Max: 0; ); Mana: (Min: 15; Max: 25; );
    Skill: (skStaff, skConcentration, skMeditation);
    Item: (itmHood, itmLight_Clothes, itmNone, itmNone, itmQuarterstaff, itmNone,
    itmYew_Wand, itmNone, itmNone, itmNone); ),
    // Ranger
    (Description: 'Ranger'; Strength: (Min: 1; Max: 2; ); Dexterity: (Min: 1;
    Max: 4; ); Willpower: (Min: 0; Max: 0; ); Perception: (Min: 0; Max: 0; );
    Life: (Min: 5; Max: 10; ); Mana: (Min: 1; Max: 5; );
    Skill: (skBow, skDodge, skDodge); Item: (itmCap, itmQuilted_Armor, itmNone, itmNone,
    itmDagger1, itmNone, itmShort_Bow, itmLight_Quiver, itmNone, itmNone); ),
    // Thief
    (Description: 'Thief'; Strength: (Min: 0; Max: 0; ); Dexterity: (Min: 1;
    Max: 2; ); Willpower: (Min: 0; Max: 0; ); Perception: (Min: 1; Max: 4; );
    Life: (Min: 5; Max: 7; ); Mana: (Min: 5; Max: 7; );
    Skill: (skDagger, skToughness, skStealth);
    Item: (itmCap, itmQuilted_Armor, itmNone, itmNone, itmDagger1, itmNone, itmNone,
    itmNone, itmNone, itmNone); )
    /// ///
    );

type
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
  S: string;
begin
  F := False;
  Result := '';
  for J := Low(ClassProp[I].Item) to High(ClassProp[I].Item) do
    if (ClassProp[I].Item[J] <> TItemEnum.itmNone) then
    begin
      S := Items.Name[ClassProp[I].Item[J]];
      Utils.AppStr(Result, S, F);
      F := True;
    end;
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

initialization

  Classes := TClasses.Create;

finalization

  FreeAndNil(Classes);

end.
