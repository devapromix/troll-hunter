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
  TClassEnum = (clWarrior, clRanger, clMonk, clMage, clRogue);

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
  dsWarrior = 'Воины — мастера ближнего боя и отлично обращаются с мечами, топорами и другим холодным оружием. ' +
    'Своими сильными ударами они могут наносить урон нескольким противникам сразу, а могут сфокусироваться на одном. ' +
    'За счет прочной брони и большого количества жизни воины могут долго удерживать натиск врага.';
  dsRanger = 'Стрелки прекрасно справляются с задачей нанесения удаленного урона по одной цели. ' +
    'Их броня сделана из легких материалов, поэтому их основная задача не подпустить врага к себе на расстояние удара. ' +
    'Основное оружие стрелков — лук. Их меткие стрелы поражают врага в самое сердце, и могут обездвижить на несколько секунд.';
  dsMonk = 'Монахи - мастера во многих видах рукопашного боя. Этому способствует отменное владение телом, духом и разумом. ' +
    'Они прекрасно тренированы как в смысле физического развития, так и в интеллектуальной сфере. ' +
    'Также они эксперты в вопросах боя с посохом, отлично умеют уходить от ударов и обладают несгибаемой волей.';
  dsMage = 'Маги обладают знаниями древних заклятий и могут призывать к себе на помощь силу стихий. ' +
    'Они способны наносить огромный урон большому количеству врагов, а также замедлять, сковывать и откидывать врагов. ' +
    'В качестве оружия маги используют магические артефакты, которые дают им силу и единение с энергетическими потоками мира.';
  dsRogue = 'Мастера совершать кражи, вскрывать замки, отыскивать и обезвреживать ловушки. ' +
    'Благодаря незаметности и разнообразным техникам боя, наносящим урон в течение времени, они способны поражать врага постоянно, ' +
    'снижая уровень здоровья соперника до тех пор, пока у бедной жертвы не пропадет воля к победе.';

const
  ClassProp: TClassArray<TClassProp> = (
    // Warrior
    (Description: dsWarrior; Strength: (Min: 1; Max: 4;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 0; Max: 0;); Perception: (Min: 0; Max: 0;);
    Life: (Min: 10; Max: 15;); Mana: (Min: 0; Max: 0;); Skill: (skBlade, skAthletics, skBodybuilding);
    Item: (ivCap, ivQuilted_Armor, None, None, ivRusty_Sword, ivBuckler, None, None);),
    // Ranger
    (Description: dsRanger; Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 4;); Willpower: (Min: 0; Max: 0;); Perception: (Min: 0; Max: 0;);
    Life: (Min: 5; Max: 10;); Mana: (Min: 1; Max: 5;); Skill: (skBow, skDodge, skDodge);
    Item: (ivCap, ivQuilted_Armor, None, None, ivShort_Bow, None, None, None);),
    // Monk
    (Description: dsMonk; Strength: (Min: 1; Max: 2;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 2; Max: 3;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 3; Max: 7;); Mana: (Min: 11; Max: 14;); Skill: (skStaff, skConcentration, skMeditation);
    Item: (ivHood, ivLight_Clothes, None, None, ivQuarterstaff, None, None, None);),
    // Mage
    (Description: dsMage; Strength: (Min: 0; Max: 0;); Dexterity: (Min: 0; Max: 0;); Willpower: (Min: 1; Max: 4;); Perception: (Min: 1; Max: 2;);
    Life: (Min: 0; Max: 0;); Mana: (Min: 15; Max: 25;); Skill: (skStaff, skConcentration, skMeditation);
    Item: (ivHood, ivLight_Clothes, None, None, ivShort_Staff, None, None, None);),
    // Rogue
    (Description: dsRogue; Strength: (Min: 0; Max: 0;); Dexterity: (Min: 1; Max: 2;); Willpower: (Min: 0; Max: 0;); Perception: (Min: 1; Max: 4;);
    Life: (Min: 5; Max: 7;); Mana: (Min: 5; Max: 7;); Skill: (skDagger, skToughness, skStealth);
    Item: (ivCap, ivQuilted_Armor, None, None, ivSharp_Dirk, None, None, None);)
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
  EnumHelper,
  Trollhunter.Helpers,
  Trollhunter.Utils,
  Trollhunter.Item,
  Trollhunter.Player,
  Trollhunter.UI,
  Trollhunter.Language;

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
