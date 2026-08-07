unit Trollhunter.StatusEffect;

interface

uses
  Trollhunter.Types;

type
  TStatusEffectEnum = (sePoisoned, seBlinded, seStunned, seBurning, seRegen,
    seSleeping, seBloodlust, seCursed, seDrunk, seDiseased, seWeak, seAfraid,
    seArmor_Reduction, seLight, seBerserk, seWeightless, seMana_Shield,
    seAiming, seSlowed, seStealth);

type
  TSetOfStatusEffect = set of TStatusEffectEnum;

type
  TStatusEffects = class(TObject)
  private
    FStatusEffectName: array [TStatusEffectEnum] of string;
    FStatusEffect: array [TStatusEffectEnum] of Int;
    function GetStatusEffect(const AStatusEffect: TStatusEffectEnum): Int;
    procedure SetStatusEffect(const AStatusEffect: TStatusEffectEnum; const Value: Int);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property StatusEffect[const AStatusEffect: TStatusEffectEnum]: Int
      read GetStatusEffect write SetStatusEffect;
    function IsStatusEffect(const AStatusEffect: TStatusEffectEnum): boolean;
    procedure Modify(const AStatusEffect: TStatusEffectEnum; const Value: Int);
    function GetName(const AStatusEffect: TStatusEffectEnum): string;
    function GetColor(const AStatusEffect: TStatusEffectEnum): string;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Trollhunter.Helpers;

  { TStatusEffect }

const
  StatusEffectColor: array [TStatusEffectEnum] of string = ('Lighter Green', 'White',
    'Dark Yellow', 'Light Red', 'Lighter Red', 'Yellow', 'Dark Red',
    'Dark Green', 'Light Blue', 'Dark Red', 'Dark White', 'Light Green',
    'Light Yellow', 'Lighter Yellow', 'Light Red', 'Light Blue', 'Lighter Blue',
    'Light Green', 'Lighter Blue', 'White');

procedure TStatusEffects.Modify(const AStatusEffect: TStatusEffectEnum;
  const Value: Int);
begin
  FStatusEffect[AStatusEffect] := FStatusEffect[AStatusEffect] + Value;
  if FStatusEffect[AStatusEffect] < 0 then
    FStatusEffect[AStatusEffect] := 0;
end;

procedure TStatusEffects.Clear;
var
  I: TStatusEffectEnum;
begin
  for I := Low(TStatusEffectEnum) to High(TStatusEffectEnum) do
    StatusEffect[I] := 0;
end;

constructor TStatusEffects.Create;
var
  LStatusEffect: TStatusEffectEnum;
  P: Pointer;
begin
  Self.Clear;
  P := TypeInfo(TStatusEffectEnum);
  for LStatusEffect := Low(TStatusEffectEnum) to High(TStatusEffectEnum) do
    FStatusEffectName[LStatusEffect] := GetEnumName(P, Ord(LStatusEffect)).GetName('se');
end;

destructor TStatusEffects.Destroy;
begin

  inherited;
end;

function TStatusEffects.GetStatusEffect(const AStatusEffect: TStatusEffectEnum): Int;
begin
  Result := FStatusEffect[AStatusEffect];
end;

function TStatusEffects.GetColor(const AStatusEffect: TStatusEffectEnum): string;
begin
  Result := StatusEffectColor[AStatusEffect];
end;

function TStatusEffects.IsStatusEffect(const AStatusEffect: TStatusEffectEnum): boolean;
begin
  Result := StatusEffect[AStatusEffect] > 0;
end;

procedure TStatusEffects.SetStatusEffect(const AStatusEffect: TStatusEffectEnum; const Value: Int);
begin
  FStatusEffect[AStatusEffect] := Value;
end;

function TStatusEffects.GetName(const AStatusEffect: TStatusEffectEnum): string;
begin
  Result := FStatusEffectName[AStatusEffect];
end;

end.
