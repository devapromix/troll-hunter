unit Trollhunter.Talent;

interface

uses
  Trollhunter.Types,
  Trollhunter.Creature;

const
  TalentMax = 10;

type
  TTalentEnum = (tlNone, tlStrong { Strong }, tlDextrous { Dextrous },
    tlMage { Mage }, tlTough { Tough }, tlWealthy { Wealthy },
    tlAffinity_with_Swords, tlAffinity_with_Axes, tlAffinity_with_Polearms,
    tlAffinity_with_Maces, tlAffinity_with_Staves, tlAffinity_with_Wands,
    tlAffinity_with_Daggers, tlAffinity_with_Bows, tlBodybuilding, tlMeditation,
    tlEnchant_Item, tlMiser { Miser }, tlCareful { Careful },
    tlIron_Skin { Iron Skin }, tlHardy { Hardy },
    tlCharged { Charged });

type
  TTalentBase = record
    Level: UInt;
    Effects: TEffects;
  end;

const
  TalentBase: array [TTalentEnum] of TTalentBase = (
    // None
    (Level: 0; Effects: []; ),
    // Strong
    (Level: 1; Effects: [efPrmAthletics]; ),
    // Dextrous
    (Level: 1; Effects: [efPrmDodge]; ),
    // Mage
    (Level: 1; Effects: [efPrmConcentration]; ),
    // Tough
    (Level: 1; Effects: [efPrmToughness]; ),
    // Wealthy
    (Level: 1; Effects: [efPrmGold]; ),
    // Affinity with Swords
    (Level: 2; Effects: [efPrmBlade]; ),
    // Affinity with Axes
    (Level: 2; Effects: [efPrmAxe]; ),
    // Affinity with Polearms
    (Level: 2; Effects: [efPrmSpear]; ),
    // Affinity with Maces
    (Level: 2; Effects: [efPrmMace]; ),
    // Affinity with Staves
    (Level: 2; Effects: [efPrmStaff]; ),
    // Affinity with Wands
    (Level: 2; Effects: [efPrmWand]; ),
    // Affinity with Daggers
    (Level: 2; Effects: [efPrmDagger]; ),
    // Affinity with Bows
    (Level: 2; Effects: [efPrmBow]; ),
    // Bodybuilding
    (Level: 3; Effects: [efPrmBodybuilding]; ),
    // Meditation
    (Level: 3; Effects: [efPrmMeditation]; ),
    // Enchant Item
    (Level: 3; Effects: [efPrmEnchant_Item]; ),
    // Miser
    (Level: 4; Effects: [ef2xGold]; ),
    // Careful
    (Level: 4; Effects: [efPrmDV]; ),
    // Iron Skin
    (Level: 4; Effects: [efPrmPV]; ),
    // Hardy
    (Level: 5; Effects: [efPrmLife]; ),
    // Charged
    (Level: 5; Effects: [efPrmMana]; )
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

const
  TalentDescription: array [TTalentEnum] of string = ('',
    'Increases Athletics skill.',
    'Increases Dodge skill.',
    'Increases Concentration skill.',
    'Increases Toughness skill.',
    'Grants extra gold at the start of the game.',
    'Increases skill with swords.',
    'Increases skill with axes.',
    'Increases skill with polearms.',
    'Increases skill with maces.',
    'Increases skill with staves.',
    'Increases skill with wands.',
    'Increases skill with daggers.',
    'Increases skill with bows.',
    'Increases Bodybuilding skill.',
    'Increases Meditation skill.',
    'Increases Enchant Item skill.',
    'Doubles the amount of gold dropped.',
    'Increases Defense Value (DV).',
    'Increases Protection Value (PV).',
    'Increases maximum Life.',
    'Increases maximum Mana.');

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
      (T <> tlNone)) then
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

function TTalents.GetDescription(I: TTalentEnum): string;
begin
  Result := TalentDescription[I];
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
