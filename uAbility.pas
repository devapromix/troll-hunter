unit uAbility;

interface

type
  TAbilityEnum = (abPoisoned, abBlinded, abStunned, abBurning, abRegen,
    abSleeping, abBloodlust, abCursed, abDrunk, abDiseased, abWeak, abAfraid);

type
  TSetOfAbility = set of TAbilityEnum;

type
  TAbilities = class(TObject)
  private
    FAbilityName: array [TAbilityEnum] of string;
    FAbility: array [TAbilityEnum] of Word;
    function GetAbility(I: TAbilityEnum): Word;
    procedure SetAbility(I: TAbilityEnum; const Value: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Ability[I: TAbilityEnum]: Word read GetAbility write SetAbility;
    function IsAbility(Value: TAbilityEnum): Boolean;
    procedure Modify(I: TAbilityEnum; Value: Integer);
    function GetName(Value: TAbilityEnum): string;
    function GetColor(Value: TAbilityEnum): string;
  end;

implementation

uses SysUtils, TypInfo;

{ TAbility }

const
  AbilityColor: array [TAbilityEnum] of string = ('Lighter Green', 'White',
    'Dark Yellow', 'Light Red', 'Lighter Red', 'Yellow', 'Dark Red',
    'Dark Green', 'Light Blue', 'Dark Red', 'Dark White', 'Light Green');

procedure TAbilities.Modify(I: TAbilityEnum; Value: Integer);
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
    FAbilityName[I] := StringReplace(GetEnumName(P, Ord(I)), 'ab', '', [rfReplaceAll]);
end;

destructor TAbilities.Destroy;
begin

  inherited;
end;

function TAbilities.GetAbility(I: TAbilityEnum): Word;
begin
  Result := FAbility[I]
end;

function TAbilities.GetColor(Value: TAbilityEnum): string;
begin
  Result := AbilityColor[Value];
end;

function TAbilities.IsAbility(Value: TAbilityEnum): Boolean;
begin
  Result := Ability[Value] > 0;
end;

procedure TAbilities.SetAbility(I: TAbilityEnum; const Value: Word);
begin
  FAbility[I] := Value;
end;

function TAbilities.GetName(Value: TAbilityEnum): string;
begin
  Result := FAbilityName[Value];
end;

end.
