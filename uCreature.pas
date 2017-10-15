unit uCreature;

interface

uses uEntity, uAbility, uAttribute;

const
  PVMax = 250;

type
  TEffect = (efLife, efMana, efFood, efTeleportation, efIdentification,
    efTownPortal, efMagicEye, efCurePoison, efVision, efCureWeak, efPrmGold,
    efPrmAthletics, efPrmDodge, efPrmConcentration, efPrmToughness, efPrmBlade,
    efPrmAxe, efPrmSpear, efPrmMace, ef2xGold, efBloodlust, efPrmLife,
    efPrmMana, efPrmDV, efPrmPV);

type
  TEffects = set of TEffect;

type
  TMinMax = record
    Min: Word;
    Max: Word;
  end;

  TDamage = TMinMax;

  TBaseDamage = record
    MinDamage: TDamage;
    MaxDamage: TDamage;
  end;

type
  TCreature = class(TEntity)
  private
    FLife: Word;
    FMaxLife: Word;
    FAbilities: TAbilities;
    FAttributes: TAttributes;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetDamage(AMin, AMax: Word);
    function GetDamage: TDamage;
    function GetRealDamage(ADamage, APV: Word): Word;
    function IsDead: Boolean;
    procedure OnTurn;
    property Life: Word read FLife write FLife;
    property MaxLife: Word read FMaxLife write FMaxLife;
    property Abilities: TAbilities read FAbilities write FAbilities;
    property Attributes: TAttributes read FAttributes write FAttributes;
  end;

implementation

uses SysUtils, Math;

{ TCreature }

procedure TCreature.Clear;
begin
  Abilities.Clear;
  Attributes.Clear;
end;

constructor TCreature.Create;
begin
  inherited;
  FAttributes := TAttributes.Create;
  FAbilities := TAbilities.Create;
end;

destructor TCreature.Destroy;
begin
  FreeAndNil(FAbilities);
  FreeAndNil(FAttributes);
  inherited;
end;

function TCreature.GetDamage: TDamage;
begin
  Result.Min := Attributes.Attrib[atMinDamage].Value;
  Result.Max := Attributes.Attrib[atMaxDamage].Value;
end;

function TCreature.GetRealDamage(ADamage, APV: Word): Word;
begin
  Result := EnsureRange(ADamage - Round((ADamage * ((APV * 100) / PVMax)) /
    100), 0, ADamage);
end;

function TCreature.IsDead: Boolean;
begin
  Result := Life = 0
end;

procedure TCreature.SetDamage(AMin, AMax: Word);
begin
  if (AMin < 1) then
    AMin := 1;
  if (AMax < 2) then
    AMax := 2;
  if (AMin >= AMax) then
    AMin := AMax - 1;
  Attributes.SetValue(atMinDamage, AMin);
  Attributes.SetValue(atMaxDamage, AMax);
end;

procedure TCreature.OnTurn;
var
  I: TAbilityEnum;
  Value: Byte;
begin
  for I := Low(TAbilityEnum) to High(TAbilityEnum) do
    if (Abilities.Ability[I] > 0) then
    begin
      if (I in [abSleeping]) then
        Continue;
      Abilities.Ability[I] := Abilities.Ability[I] - 1;
      if (I in [abPoisoned, abBurning]) and not IsDead then
      begin
        case I of
          abPoisoned:
            Value := 1;
          abBurning:
            Value := Math.RandomRange(1, 3);
        else
          Value := 0;
        end;
        if (Value > 0) then
          Life := Math.EnsureRange(Life - Value, 0, MaxLife);
      end;
    end;
end;

end.
