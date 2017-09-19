unit uCreature;

interface

uses uEntity, uAbility;

const
  PVMax = 250;

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
  TAtrEnum = (atDef, atDmMn, atDmMx, atLifeMx, atManaMx, atStr, atDex, atWil, atPer);

type
  TAtr = record
    Value: Word;
    Prm: Word;
  end;

type
  TCreature = class(TEntity)
  private
    FLife: Word;
    FMaxLife: Word;
    FDamage: TDamage;
    FAbilities: TAbilities;
    FAtr: array [TAtrEnum] of TAtr;
    function GetAtr(I: TAtrEnum): TAtr;
    procedure SetAtr(I: TAtrEnum; const Value: TAtr);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetDamage(AMin, AMax: Word);
    function GetRealDamage(ADamage, APV: Word): Word;
    function IsDead: Boolean;
    procedure OnTurn;
    property Life: Word read FLife write FLife;
    property MaxLife: Word read FMaxLife write FMaxLife;
    property Damage: TDamage read FDamage write FDamage;
    property Abilities: TAbilities read FAbilities write FAbilities;
    property Atr[I: TAtrEnum]: TAtr read GetAtr write SetAtr;
  end;

implementation

uses SysUtils, GNUGetText, Math;

{ TCreature }

procedure TCreature.Clear;
var
  I: TAtrEnum;
begin
  for I := Low(FAtr) to High(FAtr) do
  begin
    FAtr[I].Value := 0;
    FAtr[I].Prm := 0;
  end;
end;

constructor TCreature.Create;
begin
  inherited;
  FAbilities := TAbilities.Create
end;

destructor TCreature.Destroy;
begin
  FreeAndNil(FAbilities);
  inherited;
end;

function TCreature.GetAtr(I: TAtrEnum): TAtr;
begin
  Result := FAtr[I];
end;

function TCreature.GetRealDamage(ADamage, APV: Word): Word;
begin
  Result := EnsureRange(ADamage - Round((ADamage * ((APV * 100) / PVMax)) / 100), 0, ADamage);
end;

function TCreature.IsDead: Boolean;
begin
  Result := Life = 0
end;

procedure TCreature.SetAtr(I: TAtrEnum; const Value: TAtr);
begin
  FAtr[I] := Value;
end;

procedure TCreature.SetDamage(AMin, AMax: Word);
begin
  if (AMin < 1) then AMin := 1;
  if (AMax < 2) then AMax := 2;
  if (AMin >= AMax) then AMin := AMax - 1;
  FDamage.Min := AMin;
  FDamage.Max := AMax;
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
