unit Trollhunter.Talent;

interface

uses
  Trollhunter.Types,
  Trollhunter.Creature,
  Trollhunter.Player.Classes,
  Trollhunter.Player.Races;

const
  TalentMax = 30;
  TalentLevelStep = 5;

type
  TTalentEnum = (tlNone,
    tlStrong, tlDextrous, tlMage, tlTough,
    tlSword_Mastery, tlAxe_Training, tlPolearm_Dominance,
    tlMace_Crushing, tlStaff_Harmony, tlAffinity_with_Wands,
    tlDagger_Precision, tlBow_Archery, tlLong_Range, tlElven_Marksman,
    tlBodybuilding, tlMeditation, tlEnchant_Item, tlCareful, tlElven_Grace,
    tlIron_Skin, tlHardy, tlCharged, tlShadowcraft, tlAgility, tlAcrobatics,
    tlAlertness, tlEvasion, tlTreasure_Hunter, tlGold_Finder, tlSurvival);

type
  TClassSet = set of TClassEnum;
  TRaceSet = set of TRaceEnum;

const
  AllClasses = [clWarrior .. clThief];
  AllRaces = [rcHuman .. rcDwarf];

type
  TTalentBase = record
    Level: UInt;
    MaxLevel: UInt;
    Effects: TEffects;
    Classes: TClassSet;
    Races: TRaceSet;
    Description: string;
  end;

const
  TalentBase: array [TTalentEnum] of TTalentBase = (
    // None
    (Level: 0; MaxLevel: 5; Effects: []; Classes: AllClasses; Races: AllRaces;
    Description: ''),

    // Strong
    (Level: 1; MaxLevel: 5; Effects: [efPrmAthletics]; Classes: AllClasses;
    Races: [rcHuman, rcDwarf];
    Description: 'Increases Athletics skill.'),

    // Dextrous
    (Level: 1; MaxLevel: 5; Effects: [efPrmDodge]; Classes: AllClasses; Races: AllRaces;
    Description: 'Increases Dodge skill.'),

    // Mage
    (Level: 1; MaxLevel: 5; Effects: [efPrmConcentration]; Classes: AllClasses;
    Races: AllRaces;
    Description: 'Increases Concentration skill.'),

    // Tough
    (Level: 1; MaxLevel: 5; Effects: [efPrmToughness]; Classes: AllClasses;
    Races: AllRaces;
    Description: 'Increases Toughness skill.'),

    // Sword Mastery
    (Level: 1; MaxLevel: 5; Effects: [efPrmBlade]; Classes: [clWarrior]; Races: AllRaces;
    Description: 'Increases skill with swords.'),

    // Axe Training
    (Level: 1; MaxLevel: 5; Effects: [efPrmAxe]; Classes: [clWarrior]; Races: AllRaces;
    Description: 'Increases skill with axes.'),

    // Polearm Dominance
    (Level: 1; MaxLevel: 5; Effects: [efPrmSpear]; Classes: [clWarrior]; Races: AllRaces;
    Description: 'Increases skill with polearms.'),

    // Mace Crushing
    (Level: 1; MaxLevel: 5; Effects: [efPrmMace]; Classes: [clWarrior]; Races: AllRaces;
    Description: 'Increases skill with maces.'),

    // Staff Harmony
    (Level: 1; MaxLevel: 5; Effects: [efPrmStaff]; Classes: [clMage]; Races: AllRaces;
    Description: 'Increases skill with staves.'),

    // Affinity with Wands
    (Level: 1; MaxLevel: 5; Effects: [efPrmWand]; Classes: [clMage]; Races: AllRaces;
    Description: 'Increases skill with wands.'),

    // Dagger Precision
    (Level: 1; MaxLevel: 5; Effects: [efPrmDagger]; Classes: [clThief, clRanger];
    Races: AllRaces;
    Description: 'Increases skill with daggers.'),

    // Bow Archery
    (Level: 1; MaxLevel: 5; Effects: [efPrmBow]; Classes: [clRanger, clThief];
    Races: AllRaces;
    Description: 'Increases skill with bows.'),

    // Long Range
    (Level: 1; MaxLevel: 5; Effects: []; Classes: [clRanger];
    Races: AllRaces;
    Description: 'Increases the distance you can hit a target with a bow'),

    // Elven Marksman
    (Level: 1; MaxLevel: 1; Effects: []; Classes: [clRanger]; Races: [rcElf];
    Description: 'The keen eyes of the elves let their archers strike from further away'),

    // Bodybuilding
    (Level: 1; MaxLevel: 5; Effects: [efPrmBodybuilding]; Classes: [clWarrior];
    Races: [rcHuman, rcDwarf];
    Description: 'Relentless training has forged your body into a powerful weapon'),

    // Meditation
    (Level: 1; MaxLevel: 5; Effects: [efPrmMeditation]; Classes: [clMage];
    Races: AllRaces;
    Description: 'A disciplined mind grants greater control over magical energies'),

    // Enchant Item
    (Level: 1; MaxLevel: 5; Effects: [efPrmEnchant_Item]; Classes: [clMage];
    Races: [rcHuman, rcElf, rcGnome];
    Description: 'Allows you to create more powerful magical items'),

    // Careful
    (Level: 1; MaxLevel: 3; Effects: [efPrmDV]; Classes: AllClasses; Races: AllRaces;
    Description: 'Your cautious nature helps you avoid enemy attacks. Increases DV'),

    // Elven Grace
    (Level: 1; MaxLevel: 2; Effects: [efPrmDV]; Classes: AllClasses; Races: [rcElf];
    Description: 'The natural grace of the elves makes you difficult to hit'),

    // Iron Skin
    (Level: 1; MaxLevel: 3; Effects: [efPrmPV]; Classes: [clWarrior]; Races: [rcDwarf];
    Description: 'Your skin has become as hard as iron. Increases PV'),

    // Hardy
    (Level: 1; MaxLevel: 5; Effects: [efPrmLife]; Classes: AllClasses; Races: AllRaces;
    Description: 'Your exceptional endurance increases your maximum life'),

    // Charged
    (Level: 1; MaxLevel: 5; Effects: [efPrmMana]; Classes: AllClasses; Races: AllRaces;
    Description: 'Arcane energy flows through your body. Increases maximum mana'),

    // Shadowcraft
    (Level: 1; MaxLevel: 5; Effects: [efPrmStealth]; Classes: [clThief]; Races: AllRaces;
    Description: 'Years of moving unseen have honed your stealth'),

    // Agility
    (Level: 1; MaxLevel: 2; Effects: [efPrmDodge]; Classes: AllClasses;
    Races: [rcHuman, rcElf, rcGnome];
    Description: 'Allows you to react swiftly and avoid enemy attacks'),

    // Acrobatics
    (Level: 1; MaxLevel: 3; Effects: [efPrmDodge]; Classes: [clRanger]; Races: AllRaces;
    Description: 'Years of training have made your movements swift and unpredictable'),

    // Alertness
    (Level: 1; MaxLevel: 5; Effects: [efPrmAwareness]; Classes: [clThief, clRanger];
    Races: AllRaces;
    Description: 'Heightens your senses, making you more aware of your surroundings'),

    // Evasion
    (Level: 1; MaxLevel: 4; Effects: [efPrmDodge]; Classes: [clThief]; Races: [rcElf];
    Description: 'Your agility allows you to evade attacks with greater ease'),

    // Treasure Hunter
    (Level: 1; MaxLevel: 5; Effects: [efPrmTreasureHunter]; Classes: AllClasses;
    Races: [rcGnome, rcDwarf];
    Description: 'Increases the amount of gold dropped by monsters'),

    // Gold Finder
    (Level: 1; MaxLevel: 5; Effects: [efPrmGoldFinder]; Classes: AllClasses;
    Races: AllRaces;
    Description: 'Increases the chance of finding gold on defeated monsters'),

    // Survival
    (Level: 1; MaxLevel: 5; Effects: [efPrmSurvival];
    Classes: AllClasses; Races: AllRaces;
    Description: 'Your survival instincts help you make every ration last longer')
    );

type
  TTalent = record
    Enum: TTalentEnum;
    Level: UInt;
  end;

type
  TTalents = class(TObject)
  private
    FIsPoint: boolean;
    FTalent: array [0 .. TalentMax - 1] of TTalent;
    FTalentName: array [TTalentEnum] of string;
    function GetTalent(I: UInt): TTalent;
    procedure SetTalent(I: UInt; const Value: TTalent);
  public
    constructor Create;
    destructor Destroy; override;
    constructor Clear;
    property IsPoint: boolean read FIsPoint write FIsPoint;
    property Talent[I: UInt]: TTalent read GetTalent write SetTalent;
    function GetName(I: TTalentEnum): string;
    function GetDescription(I: TTalentEnum): string;
    function GetLevelName(I: TTalentEnum; ALevel: UInt): string;
    procedure Add(const ATalent: TTalentEnum);
    function IsTalent(const ATalent: TTalentEnum): boolean;
    function IsAvailable(const ATalent: TTalentEnum): boolean;
    function GetLevel(const ATalent: TTalentEnum): UInt;
    function NextLevel(const ATalent: TTalentEnum): UInt;
    function RequiredPlayerLevel(const ATalent: TTalentEnum;
      const ALevel: UInt): UInt;
    function Count: UInt;
    function Amount: UInt;
    procedure DoTalent(Key: UInt);
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Trollhunter.Skill,
  Trollhunter.Scenes,
  Trollhunter.Player,
  Trollhunter.Attribute,
  Trollhunter.Helpers;

  { TTalents }

procedure TTalents.Add(const ATalent: TTalentEnum);
var
  I: UInt;
begin
  for I := 0 to TalentMax - 1 do
    if (FTalent[I].Enum = ATalent) then
    begin
      FTalent[I].Level := Self.NextLevel(ATalent);
      Exit;
    end;
  for I := 0 to TalentMax - 1 do
    if (FTalent[I].Enum = tlNone) then
    begin
      FTalent[I].Level := Self.NextLevel(ATalent);
      FTalent[I].Enum := ATalent;
      Break;
    end;
end;

function TTalents.Count: UInt;
begin
  Result := Length(FTalent);
end;

constructor TTalents.Create;
var
  I: TTalentEnum;
  P: Pointer;
begin
  Self.Clear;
  P := TypeInfo(TTalentEnum);
  for I := Low(TTalentEnum) to High(TTalentEnum) do
    FTalentName[I] := GetEnumName(P, Ord(I)).GetName('tl');
end;

function TTalents.Amount: UInt;
begin
  Result := Ord(High(TTalentEnum)) + 1;
end;

constructor TTalents.Clear;
var
  I: UInt;
begin
  IsPoint := True;
  for I := 0 to TalentMax - 1 do
  begin
    FTalent[I].Enum := tlNone;
    FTalent[I].Level := 0;
  end;
end;

destructor TTalents.Destroy;
begin
  inherited;
end;

procedure TTalents.DoTalent(Key: UInt);
var
  K, L: UInt;
  T: TTalentEnum;
begin
  K := 0;
  for T := Low(TTalentEnum) to High(TTalentEnum) do
    if (T <> tlNone) and Self.IsAvailable(T) then
    begin
      if (Key = K) then
      begin
        L := Self.NextLevel(T);
        Self.Add(T);
        IsPoint := False;
        Player.DoEffects(TalentBase[T].Effects, 0, L);
        Break;
      end;
      Inc(K);
    end;
end;

function TTalents.GetLevel(const ATalent: TTalentEnum): UInt;
var
  I: UInt;
begin
  Result := 0;
  for I := 0 to TalentMax - 1 do
    if (FTalent[I].Enum = ATalent) and (FTalent[I].Level > Result) then
      Result := FTalent[I].Level;
end;

function TTalents.NextLevel(const ATalent: TTalentEnum): UInt;
begin
  Result := Self.GetLevel(ATalent) + 1;
end;

function TTalents.RequiredPlayerLevel(const ATalent: TTalentEnum;
  const ALevel: UInt): UInt;
begin
  Result := TalentBase[ATalent].Level + (ALevel - 1) * TalentLevelStep;
end;

function TTalents.IsAvailable(const ATalent: TTalentEnum): boolean;
var
  NL: UInt;
begin
  NL := Self.NextLevel(ATalent);
  Result := (Player.HClass in TalentBase[ATalent].Classes) and
    (Player.HRace in TalentBase[ATalent].Races) and
    (NL <= TalentBase[ATalent].MaxLevel) and
    (Player.Attributes.Attrib[atLev].Value >= Self.RequiredPlayerLevel(ATalent, NL));
end;

function TTalents.GetDescription(I: TTalentEnum): string;
begin
  Result := TalentBase[I].Description;
end;

function TTalents.GetName(I: TTalentEnum): string;
begin
  Result := FTalentName[I];
end;

function IntToRoman(Value: UInt): string;
const
  Arabic: array [0 .. 12] of UInt = (1000, 900, 500, 400, 100, 90, 50, 40,
    10, 9, 5, 4, 1);
  Roman: array [0 .. 12] of string = ('M', 'CM', 'D', 'CD', 'C', 'XC', 'L',
    'XL', 'X', 'IX', 'V', 'IV', 'I');
var
  I: UInt;
begin
  Result := '';
  if (Value = 0) then
    Exit;
  for I := 0 to 12 do
    while (Value >= Arabic[I]) do
    begin
      Result := Result + Roman[I];
      Dec(Value, Arabic[I]);
    end;
end;

function TTalents.GetLevelName(I: TTalentEnum; ALevel: UInt): string;
begin
  if (TalentBase[I].MaxLevel <= 1) then
    Result := Self.GetName(I)
  else
    Result := Self.GetName(I) + ' ' + IntToRoman(ALevel);
end;

function TTalents.GetTalent(I: UInt): TTalent;
begin
  Result := FTalent[I];
end;

function TTalents.IsTalent(const ATalent: TTalentEnum): boolean;
var
  I: UInt;
begin
  Result := False;
  for I := 0 to TalentMax - 1 do
    if (FTalent[I].Enum = ATalent) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TTalents.SetTalent(I: UInt; const Value: TTalent);
begin
  FTalent[I] := Value;
end;

end.
