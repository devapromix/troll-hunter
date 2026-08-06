unit Trollhunter.StatusEffect;

interface

uses
  Trollhunter.Types;

type
  TStatusEffectEnum = (sePoisoned, seBlinded, seStunned, seBurning, seRegen,
    seSleeping, seBloodlust, seCursed, seDrunk, seDiseased, seWeak, seAfraid,
    seArmor_Reduction, seLight, seBerserk, seWeightless, seMana_Shield);

type
  TSetOfStatusEffect = set of TStatusEffectEnum;

type
  TStatusEffects = class(TObject)
  private
    FStatusEffectName: array [TStatusEffectEnum] of string;
    FStatusEffect: array [TStatusEffectEnum] of Int;
    function GetStatusEffect(const I: TStatusEffectEnum): Int;
    procedure SetStatusEffect(const I: TStatusEffectEnum; const Value: Int);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property StatusEffect[const I: TStatusEffectEnum]: Int read GetStatusEffect
      write SetStatusEffect;
    function IsStatusEffect(const Value: TStatusEffectEnum): Boolean;
    procedure Modify(const I: TStatusEffectEnum; const Value: Int);
    function GetName(const Value: TStatusEffectEnum): string;
    function GetColor(const Value: TStatusEffectEnum): string;
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
    'Light Yellow', 'Lighter Yellow', 'Light Red', 'Light Blue', 'Lighter Blue');

procedure TStatusEffects.Modify(const I: TStatusEffectEnum; const Value: Int);
begin
  FStatusEffect[I] := FStatusEffect[I] + Value;
  if FStatusEffect[I] < 0 then
    FStatusEffect[I] := 0;
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
  I: TStatusEffectEnum;
  P: Pointer;
begin
  Self.Clear;
  P := TypeInfo(TStatusEffectEnum);
  for I := Low(TStatusEffectEnum) to High(TStatusEffectEnum) do
    FStatusEffectName[I] := GetEnumName(P, Ord(I)).GetName('ab');
end;

destructor TStatusEffects.Destroy;
begin

  inherited;
end;

function TStatusEffects.GetStatusEffect(const I: TStatusEffectEnum): Int;
begin
  Result := FStatusEffect[I]
end;

function TStatusEffects.GetColor(const Value: TStatusEffectEnum): string;
begin
  Result := StatusEffectColor[Value];
end;

function TStatusEffects.IsStatusEffect(const Value: TStatusEffectEnum): Boolean;
begin
  Result := StatusEffect[Value] > 0;
end;

procedure TStatusEffects.SetStatusEffect(const I: TStatusEffectEnum; const Value: Int);
begin
  FStatusEffect[I] := Value;
end;

function TStatusEffects.GetName(const Value: TStatusEffectEnum): string;
begin
  Result := FStatusEffectName[Value];
end;

end.
