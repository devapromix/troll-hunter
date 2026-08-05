unit Trollhunter.Creature;

interface

uses
  Trollhunter.Types,
  Trollhunter.Entity,
  Trollhunter.Ability,
  Trollhunter.StatusEffect,
  Trollhunter.Attribute;

const
  PVMax = 250;
  MaxDamMax = 255;
  MinDamMax = MaxDamMax - 1;
  ExtraGoldMax = 200;

type
  TEffect = (efNone, efLife, efMana, efFood, efTeleportation, efIdentification,
  efAllIdentification, efEnchantItem, efRechargeWand, efCraftStr,
    efCraftDex, efCraftWil, efCraftPer, efCraftAtr, efTownPortal,efRegeneration,
    efCurePoison, efVision, efCureWeak,
    efPrmAthletics, efPrmDodge, efPrmConcentration, efPrmToughness, efPrmBlade,
    efPrmAxe, efPrmSpear,
    efPrmMace, efPrmStaff, efPrmWand, efPrmDagger, efPrmBow, efBloodlust,
    efPrmLife, efPrmMana, efPrmDV,
    efPrmPV, efPrmStr, efPrmDex, efPrmWil, efPrmPer, efRepair,
    efPrmBodybuilding, efPrmMeditation,
    efPrmEnchant_Item, efLight, efBerserk, efPrmStealth, efPrmAwareness,
    efPrmTreasureHunter, efPrmGoldFinder, efPrmSurvival, efCharges, efDisenchant,
    efPoisonWeapon, efPrmPoisoning, efManaShield, efWeaken, efBurn, efDrain);

const
  CraftEffLow = efCraftStr;
  CraftEffHigh = efCraftAtr;

const
  EfNameStr: array [CraftEffLow .. Pred(CraftEffHigh)] of string =
    ('Strength', 'Dexterity', 'Willpower', 'Perception');

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
    FStatusEffects: TStatusEffects;
    FAttributes: TAttributes;
  public
    Light: Int;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetDamage(AMin, AMax: UInt);
    function GetDamage: TDamage;
    function GetRealDamage(ADamage, APV: UInt): UInt;
    function IsDead: Boolean;
    function OnTurn: Boolean;
    property StatusEffects: TStatusEffects read FStatusEffects write FStatusEffects;
    property Attributes: TAttributes read FAttributes write FAttributes;
    procedure Fill;
  end;

implementation

uses
  SysUtils,
  Math,
  Trollhunter.Helpers;

{ TCreature }

procedure TCreature.Clear;
begin
  StatusEffects.Clear;
  Attributes.Clear;
end;

constructor TCreature.Create;
begin
  inherited;
  Light := 0;
  FAttributes := TAttributes.Create;
  FStatusEffects := TStatusEffects.Create;
end;

destructor TCreature.Destroy;
begin
  FreeAndNil(FStatusEffects);
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
  I: TStatusEffectEnum;
  Value: UInt;
begin
  Result := False;
  for I := Low(TStatusEffectEnum) to High(TStatusEffectEnum) do
    if (StatusEffects.StatusEffect[I] > 0) then
    begin
      if (I in [seSleeping]) then
        Continue;
      StatusEffects.Modify(I, -1);
      if (StatusEffects.StatusEffect[I] = 0) then
        Result := True;
      if (I in [sePoisoned, seBurning]) and not IsDead then
      begin
        case I of
          sePoisoned:
            Value := 1;
          seBurning:
            Value := Math.RandomRange(1, 3);
        else
          Value := 0;
        end;
        if (Value > 0) then
          Attributes.Modify(atLife, -Value);
      end;
    end;
  if (Light > 0) then
    Result := True;
end;

end.
