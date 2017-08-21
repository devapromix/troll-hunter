unit uAbility;

interface

type
  TAbilityEnum = (abPoisoned, abBlinded, abStunned, abBurning, abRegen,
    abSleeping);

type
  TAbilities = class(TObject)
  private
    FAbility: array [TAbilityEnum] of Word;
    function GetAbility(I: TAbilityEnum): Word;
    procedure SetAbility(I: TAbilityEnum; const Value: Word);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Ability[I: TAbilityEnum]: Word read GetAbility write SetAbility;
    function IsAbility(Value: TAbilityEnum): Boolean;
    procedure Append(I: TAbilityEnum; Value: Word);
  end;

implementation

{ TAbility }

procedure TAbilities.Append(I: TAbilityEnum; Value: Word);
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
begin
  Self.Clear;
end;

destructor TAbilities.Destroy;
begin

  inherited;
end;

function TAbilities.GetAbility(I: TAbilityEnum): Word;
begin
  Result := FAbility[I]
end;

function TAbilities.IsAbility(Value: TAbilityEnum): Boolean;
begin
  Result := Ability[Value] > 0;
end;

procedure TAbilities.SetAbility(I: TAbilityEnum; const Value: Word);
begin
  FAbility[I] := Value;
end;

end.
