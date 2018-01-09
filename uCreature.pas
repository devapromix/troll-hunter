unit uCreature;

interface

uses uTypes, uEntity, uAbility, uAttribute;

const
  PVMax = 250;
  MaxDamMax = 255;
  MinDamMax = MaxDamMax - 1;
  ExtraGoldMax = 200;

type
  TEffect = (efLife, efMana, efFood, efTeleportation, efIdentification, efAllIdentification, efCraftStr, efCraftDex, efCraftWil, efCraftPer,
    efCraftAtr, efTownPortal, efMagicEye, efCurePoison, efVision, efCureWeak, efPrmGold, efPrmAthletics, efPrmDodge,
    efPrmConcentration, efPrmToughness, efPrmBlade, efPrmAxe, efPrmSpear, efPrmMace, efPrmStaff, efPrmWand, efPrmDagger,
    efPrmBow, ef2xGold, efBloodlust, efPrmLife, efPrmMana, efPrmDV, efPrmPV, efPrmStr, efPrmDex, efPrmWil, efPrmPer,
    efRepair);

const
  CraftEffLow = efCraftStr;
  CraftEffHigh = efCraftAtr;

const
  EfNameStr: array [CraftEffLow .. Pred(CraftEffHigh)] of string = ('Strength', 'Dexterity', 'Willpower', 'Perception');

type
  TEffects = set of TEffect;

type
  TMinMax = record
    Min: UInt;
    Max: UInt;
  end;

  TDamage = TMinMax;

  TBaseDamage = record
    MinDamage: TDamage;
    MaxDamage: TDamage;
  end;

type
  TCreature = class(TEntity)
  private
    FAbilities: TAbilities;
    FAttributes: TAttributes;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetDamage(AMin, AMax: UInt);
    function GetDamage: TDamage;
    function GetRealDamage(ADamage, APV: UInt): UInt;
    function IsDead: Boolean;
    function OnTurn: Boolean;
    property Abilities: TAbilities read FAbilities write FAbilities;
    property Attributes: TAttributes read FAttributes write FAttributes;
    procedure Fill;
  end;

implementation

uses SysUtils, Math, uHelpers, uGame;

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

procedure TCreature.Fill;
begin
  Attributes.SetValue(atLife, atMaxLife);
  Attributes.SetValue(atMana, atMaxMana);
end;

function TCreature.GetDamage: TDamage;
begin
  Result.Min := EnsureRange(Attributes.Attrib[atMinDamage].Value, 1, MinDamMax);
  Result.Max := EnsureRange(Attributes.Attrib[atMaxDamage].Value, 2, MaxDamMax);
end;

function TCreature.GetRealDamage(ADamage, APV: UInt): UInt;
var
  Value: UInt;
begin
  Value := Round(ADamage * ((APV * 100) / PVMax) / 100);
  Result := ADamage - Value.InRange(ADamage);
end;

function TCreature.IsDead: Boolean;
begin
  Result := (Attributes.Attrib[atLife].Value = 0);
end;

procedure TCreature.SetDamage(AMin, AMax: UInt);
begin
  AMin := EnsureRange(AMin, 1, MinDamMax);
  AMax := EnsureRange(AMax, 2, MaxDamMax);
  if (AMin >= AMax) then
    AMin := AMax - 1;
  Attributes.SetValue(atMinDamage, AMin);
  Attributes.SetValue(atMaxDamage, AMax);
end;

function TCreature.OnTurn: Boolean;
var
  I: TAbilityEnum;
  Value: UInt;
begin
  Result := False;
  for I := Low(TAbilityEnum) to High(TAbilityEnum) do
    if (Abilities.Ability[I] > 0) then
    begin
      if (I in [abSleeping]) then
        Continue;
      Abilities.Modify(I, -1);
      if (Abilities.Ability[I] = 0) then
        Result := True;
      if (I in [abPoisoned, abBurning]) and not IsDead then
      begin
        case I of
          abPoisoned:
            Value := 1;
          abBurning:
            Value := Math.RandomRange(1, 2);
        else
          Value := 0;
        end;
        if (Value > 0) then
          Attributes.Modify(atLife, -Value);
      end;
    end;
end;

end.
