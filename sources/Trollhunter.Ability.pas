unit Trollhunter.Ability;

interface

uses
  Trollhunter.Types;

type
  TAbilityEnum = (abPoisoned, abBlinded, abStunned, abBurning, abRegen, abSleeping, abBloodlust, abCursed, abDrunk, abDiseased, abWeak, abAfraid,
    abArmor_Reduction, abLight, abBerserk, abWeightless);

type
  TSetOfAbility = set of TAbilityEnum;

type
  TAbility<T> = array [TAbilityEnum] of T;

type
  TAbilities = class(TObject)
  private
    FAbilityName: TAbility<string>;
    FAbility: TAbility<UInt>;
    function GetAbility(const I: TAbilityEnum): UInt;
    procedure SetAbility(const I: TAbilityEnum; const Value: UInt);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Ability[const I: TAbilityEnum]: UInt read GetAbility write SetAbility;
    function IsAbility(const Value: TAbilityEnum): Boolean;
    procedure Modify(const I: TAbilityEnum; const Value: Int);
    function GetName(const Value: TAbilityEnum): string;
    function GetColor(const Value: TAbilityEnum): string;
  end;

implementation

uses
  SysUtils,
  TypInfo,
  Trollhunter.Helpers,
  EnumHelper;

{ TAbility }

const
  AbilityColor: TAbility<string> = ('Lighter Green', 'White', 'Dark Yellow', 'Light Red', 'Lighter Red', 'Yellow', 'Dark Red', 'Dark Green',
    'Light Blue', 'Dark Red', 'Dark White', 'Light Green', 'Light Yellow', 'Lighter Yellow', 'Light Red', 'Light Blue');

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
begin
  Self.Clear;
  for I := Low(TAbilityEnum) to High(TAbilityEnum) do
    FAbilityName[I] := Enum<TAbilityEnum>.ValueName(I).GetName('ab');
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
