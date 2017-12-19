unit uTalent;

interface

uses uCreature;

const
  TalentMax = 10;

type
  TTalentEnum = (tlNone, tlStrong { Сильный } , tlDextrous { Ловкий } ,
    tlMage { Маг } , tlTough { Тяжелый } , tlWealthy { Богатый } ,
    tlAffinity_with_Swords, tlAffinity_with_Axes,
    tlAffinity_with_Polearms, tlAffinity_with_Maces, tlAffinity_with_Staves,
    tlMiser { Скряга }, tlCareful { Осторожный }, tlIron_Skin { Железная Кожа },
    tlHardy { Выносливый }, tlCharged { Энергичный } );

type
  TTalentBonus = (tbNone, tbAttrib, tbSkill, tbTalent, tbGold);

type
  TTalentBase = record
    Level: Byte;
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
    (Level: 3; TalentBonus: tbSkill; Effects: [efPrmBlade];),
    // Affinity with Axes
    (Level: 3; TalentBonus: tbSkill; Effects: [efPrmAxe];),
    // Affinity with Polearms
    (Level: 3; TalentBonus: tbSkill; Effects: [efPrmSpear];),
    // Affinity with Maces
    (Level: 3; TalentBonus: tbSkill; Effects: [efPrmMace];),
    // Affinity with Staves
    (Level: 3; TalentBonus: tbSkill; Effects: [efPrmStaff];),
    // Miser
    (Level: 5; TalentBonus: tbNone; Effects: [ef2xGold];),
    // Careful
    (Level: 5; TalentBonus: tbTalent; Effects: [efPrmDV];),
    // Iron Skin
    (Level: 5; TalentBonus: tbTalent; Effects: [efPrmPV];),
    // Hardy
    (Level: 5; TalentBonus: tbAttrib; Effects: [efPrmLife];),
    // Charged
    (Level: 5; TalentBonus: tbAttrib; Effects: [efPrmMana];));

type
  TTalent = record
    Enum: TTalentEnum;
    Level: Byte;
  end;

type
  TTalents = class(TObject)
  private
    FIsPoint: Boolean;
    FTalent: array [0 .. TalentMax - 1] of TTalent;
    FTalentName: array [TTalentEnum] of string;
    function GetTalent(I: Byte): TTalent;
    procedure SetTalent(I: Byte; const Value: TTalent);
  public
    constructor Create;
    destructor Destroy; override;
    constructor Clear;
    property IsPoint: Boolean read FIsPoint write FIsPoint;
    property Talent[I: Byte]: TTalent read GetTalent write SetTalent;
    function GetName(I: TTalentEnum): string;
    function GetHint(I: TTalentEnum): string;
    procedure Add(const ATalent: TTalentEnum);
    function IsTalent(const ATalent: TTalentEnum): Boolean;
    function Count: Byte;
    function Amount: Byte;
    procedure DoTalent(Key: Byte);
  end;

implementation

uses SysUtils, TypInfo, uLanguage, uSkill, uGame, uScenes, uPlayer, uAttribute;

const
  TalentHint: array [TTalentEnum] of string = (
  '', 'Athletics', 'Dodge', 'Concentration', 'Toughness',
  'Gold', 'Blade', 'Axe', 'Spear', 'Mace', 'Staff',
  'x2 to Gold', 'DV', 'PV', 'Life', 'Mana'
  );

{ TTalents }

procedure TTalents.Add(const ATalent: TTalentEnum);
var
  I: Byte;
begin
  for I := 0 to TalentMax - 1 do
    if (FTalent[I].Enum = tlNone) then
    begin
      FTalent[I].Enum := ATalent;
      Break;
    end;
end;

function TTalents.Count: Byte;
begin
  Result := Length(FTalent);
end;

constructor TTalents.Create;
var
  I: TTalentEnum;
  P: Pointer;
  S: string;
begin
  Self.Clear;
  P := TypeInfo(TTalentEnum);
  for I := Low(TTalentEnum) to High(TTalentEnum) do
  begin
    S := StringReplace(GetEnumName(P, Ord(I)), 'tl', '', [rfReplaceAll]);
    S := StringReplace(S, '_', ' ', [rfReplaceAll]);
    FTalentName[I] := S;
  end;
end;

function TTalents.Amount: Byte;
begin
  Result := Ord(High(TTalentEnum)) + 1;
end;

constructor TTalents.Clear;
var
  I: Byte;
begin
  IsPoint := True;
  for I := 0 to TalentMax - 1 do
    FTalent[I].Enum := tlNone;
end;

destructor TTalents.Destroy;
begin

  inherited;
end;

procedure TTalents.DoTalent(Key: Byte);
var
  T: TTalentEnum;
  K: Byte;
begin
  K := 0;
  for T := Low(TTalentEnum) to High(TTalentEnum) do
    if ((TalentBase[T].Level = Player.Attributes.Attrib[atLev].Value)
      and (T <> tlNone)) then
    begin
      if (Key = K) then
      begin
        Self.Add(T);
        IsPoint := False;
        Player.DoEffects(TalentBase[T].Effects);
        if not Mode.Game then
          Scenes.SetScene(scName);
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

function TTalents.GetTalent(I: Byte): TTalent;
begin
  Result := FTalent[I]
end;

function TTalents.IsTalent(const ATalent: TTalentEnum): Boolean;
var
  I: Byte;
begin
  Result := False;
  for I := 0 to TalentMax - 1 do
    if (FTalent[I].Enum = ATalent) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TTalents.SetTalent(I: Byte; const Value: TTalent);
begin
  FTalent[I] := Value;
end;

end.
