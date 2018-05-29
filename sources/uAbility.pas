unit uAbility;

interface

uses uTypes;

type
  TAbilityEnum = (abPoisoned, abBlinded, abStunned, abBurning, abRegen,
    abSleeping, abBloodlust, abCursed, abDrunk, abDiseased, abWeak, abAfraid,
    abArmor_Reduction, abLight, abBerserk);

type
  TSetOfAbility = set of TAbilityEnum;

type
  TAbilities = class(TObject)
  private
    FAbilityName: array [TAbilityEnum] of string;
    FAbility: array [TAbilityEnum] of UInt;
    function GetAbility(const I: TAbilityEnum): UInt;
    procedure SetAbility(const I: TAbilityEnum; const Value: UInt);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Ability[const I: TAbilityEnum]: UInt read GetAbility
      write SetAbility;
    function IsAbility(const Value: TAbilityEnum): Boolean;
    procedure Modify(const I: TAbilityEnum; const Value: Int);
    function GetName(const Value: TAbilityEnum): string;
    function GetColor(const Value: TAbilityEnum): string;
  end;

implementation

uses SysUtils, TypInfo, uHelpers;

{ TAbility }

const
  AbilityColor: array [TAbilityEnum] of string = ('Lighter Green', 'White',
    'Dark Yellow', 'Light Red', 'Lighter Red', 'Yellow', 'Dark Red',
    'Dark Green', 'Light Blue', 'Dark Red', 'Dark White', 'Light Green',
    'Light Yellow', 'Lighter Yellow', 'Light Red');

procedure TAbilities.Modify(const I: TAbilityEnum; const Value: Int);
begin
  FAbility[I] := FAbility[I] + Value;
end;

procedure TAbilities.Clear;
var
  I: TAbilityEnum;
begin
  for I := Low(TAbilityEnum) to High(TAbilityEnum) do
    Ability[I] := 0;
end;

constructor TAbilities.Create;
var
  I: TAbilityEnum;
  P: Pointer;
begin
  Self.Clear;
  P := TypeInfo(TAbilityEnum);
  for I := Low(TAbilityEnum) to High(TAbilityEnum) do
    FAbilityName[I] := GetEnumName(P, Ord(I)).GetName('ab');
end;

destructor TAbilities.Destroy;
begin

  inherited;
end;

function TAbilities.GetAbility(const I: TAbilityEnum): UInt;
begin
  Result := FAbility[I]
end;

function TAbilities.GetColor(const Value: TAbilityEnum): string;
begin
  Result := AbilityColor[Value];
end;

function TAbilities.IsAbility(const Value: TAbilityEnum): Boolean;
begin
  Result := Ability[Value] > 0;
end;

procedure TAbilities.SetAbility(const I: TAbilityEnum; const Value: UInt);
begin
  FAbility[I] := Value;
end;

function TAbilities.GetName(const Value: TAbilityEnum): string;
begin
  Result := FAbilityName[Value];
end;

end.
