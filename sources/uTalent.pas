unit uTalent;

interface

uses uTypes, uCreature;

const
  TalentMax = 10;

type
  TTalentEnum = (tlNone, tlStrong { Сильный } , tlDextrous { Ловкий } , tlMage { Маг } , tlTough { Тяжелый } ,
    tlWealthy { Богатый } , tlAffinity_with_Swords, tlAffinity_with_Axes, tlAffinity_with_Polearms,
    tlAffinity_with_Maces, tlAffinity_with_Staves, tlAffinity_with_Wands, tlAffinity_with_Daggers, tlAffinity_with_Bows,
    tlBodybuilding, tlMeditation, tlEnchant_Item, tlMiser { Скряга } , tlCareful { Осторожный } , tlIron_Skin { Железная Кожа } ,
    tlHardy { Выносливый } , tlCharged { Энергичный } );

type
  TTalentBonus = (tbNone, tbAttrib, tbSkill, tbTalent, tbGold);

type
  TTalentBase = record
    Level: UInt;
    TalentBonus: TTalentBonus;
    Effects: TEffects;
  end;

const
  TalentBase: array [TTalentEnum] of TTalentBase = (
    // None
    (Level: 0; TalentBonus: tbNone; Effects: [];),
    // Strong
    (Level: 1; TalentBonus: tbSkill; Effects: [efPrmAthletics];),
    // Dextrous
    (Level: 1; TalentBonus: tbSkill; Effects: [efPrmDodge];),
    // Mage
    (Level: 1; TalentBonus: tbSkill; Effects: [efPrmConcentration];),
    // Tough
    (Level: 1; TalentBonus: tbSkill; Effects: [efPrmToughness];),
    // Wealthy
    (Level: 1; TalentBonus: tbGold; Effects: [efPrmGold];),
    // Affinity with Swords
    (Level: 2; TalentBonus: tbSkill; Effects: [efPrmBlade];),
    // Affinity with Axes
    (Level: 2; TalentBonus: tbSkill; Effects: [efPrmAxe];),
    // Affinity with Polearms
    (Level: 2; TalentBonus: tbSkill; Effects: [efPrmSpear];),
    // Affinity with Maces
    (Level: 2; TalentBonus: tbSkill; Effects: [efPrmMace];),
    // Affinity with Staves
    (Level: 2; TalentBonus: tbSkill; Effects: [efPrmStaff];),
    // Affinity with Wands
    (Level: 2; TalentBonus: tbSkill; Effects: [efPrmWand];),
    // Affinity with Daggers
    (Level: 2; TalentBonus: tbSkill; Effects: [efPrmDagger];),
    // Affinity with Bows
    (Level: 2; TalentBonus: tbSkill; Effects: [efPrmBow];),
    // Bodybuilding
    (Level: 3; TalentBonus: tbSkill; Effects: [efPrmBodybuilding];),
    // Meditation
    (Level: 3; TalentBonus: tbSkill; Effects: [efPrmMeditation];),
    // Enchant Item
    (Level: 3; TalentBonus: tbSkill; Effects: [efPrmEnchant_Item];),
    // Miser
    (Level: 4; TalentBonus: tbNone; Effects: [ef2xGold];),
    // Careful
    (Level: 4; TalentBonus: tbTalent; Effects: [efPrmDV];),
    // Iron Skin
    (Level: 4; TalentBonus: tbTalent; Effects: [efPrmPV];),
    // Hardy
    (Level: 5; TalentBonus: tbAttrib; Effects: [efPrmLife];),
    // Charged
    (Level: 5; TalentBonus: tbAttrib; Effects: [efPrmMana];)
    //
    );

type
  TTalent = record
    Enum: TTalentEnum;
    Level: UInt;
  end;

type
  TTalents = class(TObject)
  private
    FIsPoint: Boolean;
    FTalent: array [0 .. TalentMax - 1] of TTalent;
    FTalentName: array [TTalentEnum] of string;
    function GetTalent(I: UInt): TTalent;
    procedure SetTalent(I: UInt; const Value: TTalent);
  public
    constructor Create;
    destructor Destroy; override;
    constructor Clear;
    property IsPoint: Boolean read FIsPoint write FIsPoint;
    property Talent[I: UInt]: TTalent read GetTalent write SetTalent;
    function GetName(I: TTalentEnum): string;
    function GetHint(I: TTalentEnum): string;
    procedure Add(const ATalent: TTalentEnum);
    function IsTalent(const ATalent: TTalentEnum): Boolean;
    function Count: UInt;
    function Amount: UInt;
    procedure DoTalent(Key: UInt);
  end;

implementation

uses SysUtils, TypInfo, uLanguage, uSkill, uGame, uScenes, uPlayer, uAttribute, uHelpers;

const
  TalentHint: array [TTalentEnum] of string = ('', 'Athletics', 'Dodge', 'Concentration', 'Toughness', 'Gold', 'Blade',
    'Axe', 'Spear', 'Mace', 'Staff', 'Wand', 'Dagger', 'Bow', 'Bodybuilding', 'Meditation', 'Enchant Item', 'x2 to Gold', 'DV', 'PV', 'Life', 'Mana');

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
    if ((TalentBase[T].Level = Player.Attributes.Attrib[atLev].Value) and (T <> tlNone)) then
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

function TTalents.GetHint(I: TTalentEnum): string;
const
  F = '+%d to %s';
begin
  case TalentBase[I].TalentBonus of
    tbNone:
      Result := _(TalentHint[I]);
    tbGold:
      Result := Format(F, [StartGold, _(TalentHint[I])]);
    tbSkill:
      Result := Format(F, [StartSkill, _(TalentHint[I])]);
    tbTalent:
      Result := Format(F, [TalentPrm, _(TalentHint[I])]);
    tbAttrib:
      Result := Format(F, [AttribPrm, _(TalentHint[I])]);
  else
    Result := '-';
  end;
end;

function TTalents.GetName(I: TTalentEnum): string;
begin
  Result := FTalentName[I]
end;

function TTalents.GetTalent(I: UInt): TTalent;
begin
  Result := FTalent[I]
end;

function TTalents.IsTalent(const ATalent: TTalentEnum): Boolean;
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
