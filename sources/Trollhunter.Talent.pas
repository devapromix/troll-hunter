unit Trollhunter.Talent;

interface

uses
  Trollhunter.Types,
  Trollhunter.Creature,
  Trollhunter.Player.Classes,
  Trollhunter.Player.Races;

const
  TalentMax = 30;

type
  TTalentEnum = (tlNone,
    tlStrong, tlDextrous,
    tlMage, tlTough,
    tlSword_Mastery, tlAffinity_with_Axes, tlAffinity_with_Polearms,
    tlAffinity_with_Maces, tlAffinity_with_Staves, tlAffinity_with_Wands,
    tlAffinity_with_Daggers, tlAffinity_with_Bows, tlBodybuilding, tlMeditation,
    tlEnchant_Item, tlCareful,
    tlIron_Skin, tlHardy,
    tlCharged);

type
  TClassSet = set of TClassEnum;
  TRaceSet = set of TRaceEnum;

const
  AllClasses = [clWarrior .. clThief];
  AllRaces = [rcHuman .. rcDwarf];

type
  TTalentBase = record
    Level: UInt;
    Effects: TEffects;
    Classes: TClassSet;
    Races: TRaceSet;
    Description: string;        // ← Added
  end;

const
  TalentBase: array [TTalentEnum] of TTalentBase = (
    // None
    (Level: 0; Effects: []; Classes: AllClasses; Races: AllRaces;
     Description: ''),

    // Strong
    (Level: 1; Effects: [efPrmAthletics]; Classes: AllClasses; Races: AllRaces;
     Description: 'Increases Athletics skill.'),

    // Dextrous
    (Level: 1; Effects: [efPrmDodge]; Classes: AllClasses; Races: AllRaces;
     Description: 'Increases Dodge skill.'),

    // Mage
    (Level: 1; Effects: [efPrmConcentration]; Classes: AllClasses; Races: AllRaces;
     Description: 'Increases Concentration skill.'),

    // Tough
    (Level: 1; Effects: [efPrmToughness]; Classes: AllClasses; Races: AllRaces;
     Description: 'Increases Toughness skill.'),

    // Sword Mastery
    (Level: 2; Effects: [efPrmBlade]; Classes: [clWarrior]; Races: AllRaces;
     Description: 'Increases skill with swords.'),

    // Affinity with Axes
    (Level: 2; Effects: [efPrmAxe]; Classes: [clWarrior]; Races: AllRaces;
     Description: 'Increases skill with axes.'),

    // Affinity with Polearms
    (Level: 2; Effects: [efPrmSpear]; Classes: [clWarrior]; Races: AllRaces;
     Description: 'Increases skill with polearms.'),

    // Affinity with Maces
    (Level: 2; Effects: [efPrmMace]; Classes: [clWarrior]; Races: AllRaces;
     Description: 'Increases skill with maces.'),

    // Affinity with Staves
    (Level: 2; Effects: [efPrmStaff]; Classes: [clMage]; Races: AllRaces;
     Description: 'Increases skill with staves.'),

    // Affinity with Wands
    (Level: 2; Effects: [efPrmWand]; Classes: [clMage]; Races: AllRaces;
     Description: 'Increases skill with wands.'),

    // Affinity with Daggers
    (Level: 2; Effects: [efPrmDagger]; Classes: [clThief, clRanger]; Races: AllRaces;
     Description: 'Increases skill with daggers.'),

    // Affinity with Bows
    (Level: 2; Effects: [efPrmBow]; Classes: [clRanger, clThief]; Races: AllRaces;
     Description: 'Increases skill with bows.'),

    // Bodybuilding
    (Level: 3; Effects: [efPrmBodybuilding]; Classes: [clWarrior]; Races: AllRaces;
     Description: 'Increases Bodybuilding skill.'),

    // Meditation
    (Level: 3; Effects: [efPrmMeditation]; Classes: [clMage]; Races: AllRaces;
     Description: 'Increases Meditation skill.'),

    // Enchant Item
    (Level: 3; Effects: [efPrmEnchant_Item]; Classes: [clMage]; Races: AllRaces;
     Description: 'Increases Enchant Item skill.'),

    // Careful
    (Level: 4; Effects: [efPrmDV]; Classes: AllClasses; Races: AllRaces;
     Description: 'Increases Defense Value (DV).'),

    // Iron Skin
    (Level: 4; Effects: [efPrmPV]; Classes: [clWarrior]; Races: AllRaces;
     Description: 'Increases Protection Value (PV).'),

    // Hardy
    (Level: 5; Effects: [efPrmLife]; Classes: AllClasses; Races: AllRaces;
     Description: 'Increases maximum Life.'),

    // Charged
    (Level: 5; Effects: [efPrmMana]; Classes: [clMage]; Races: AllRaces;
     Description: 'Increases maximum Mana.')
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
    procedure Add(const ATalent: TTalentEnum);
    function IsTalent(const ATalent: TTalentEnum): boolean;
    function IsAvailable(const ATalent: TTalentEnum): boolean;
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
    if (FTalent[I].Enum = tlNone) then
    begin
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
    FTalent[I].Enum := tlNone;
end;

destructor TTalents.Destroy;
begin
  inherited;
end;

procedure TTalents.DoTalent(Key: UInt);
var
  K: UInt;
  T: TTalentEnum;
begin
  K := 0;
  for T := Low(TTalentEnum) to High(TTalentEnum) do
    if ((TalentBase[T].Level = Player.Attributes.Attrib[atLev].Value) and
      (T <> tlNone) and Self.IsAvailable(T)) then
    begin
      if (Key = K) then
      begin
        Self.Add(T);
        IsPoint := False;
        Player.DoEffects(TalentBase[T].Effects);
        Break;
      end;
      Inc(K);
    end;
end;

function TTalents.IsAvailable(const ATalent: TTalentEnum): boolean;
begin
  Result := (Player.HClass in TalentBase[ATalent].Classes) and
    (Player.HRace in TalentBase[ATalent].Races);
end;

function TTalents.GetDescription(I: TTalentEnum): string;
begin
  Result := TalentBase[I].Description;   // ← Updated
end;

function TTalents.GetName(I: TTalentEnum): string;
begin
  Result := FTalentName[I];
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
