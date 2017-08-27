unit uSkill;

interface

type
  TSkillEnum = (skLearning,
    // Attributes skills
    skAthletics, skDodge, skConcentration, skToughness,
    // Weapon skills
    skBlade, skAxe, skSpear, skMace,
    // Skills
    skStealth, skHealing);

type
  TSkill = record
    Value: Integer;
    Exp: Integer;
  end;

type
  TSkills = class(TObject)
  private
    FSkill: array [TSkillEnum] of TSkill;
    function GetSkill(I: TSkillEnum): TSkill;
    procedure SetSkill(I: TSkillEnum; const Value: TSkill);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Skill[I: TSkillEnum]: TSkill read GetSkill write SetSkill;
    procedure DoSkill(ASkill: TSkillEnum; AExpValue: Byte = 1);
    procedure Modify(I: TSkillEnum; Value: Integer);
    function GetName(I: TSkillEnum): string;
    procedure Start;
  end;

const
  SkillMin = 5;
  SkillMax = 75;
  SkillExpMax = 50;
  StartSkill = 5;

implementation

uses SysUtils, Math, GNUGetText, uTerminal, uPlayer, uGame, uMsgLog;

{ TSkills }

procedure TSkills.Clear;
var
  I: TSkillEnum;
begin
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    with FSkill[I] do
    begin
      Value := SkillMin;
      Exp := 0;
    end;
end;

constructor TSkills.Create;
begin
  Self.Clear
end;

destructor TSkills.Destroy;
begin

  inherited;
end;

procedure TSkills.DoSkill(ASkill: TSkillEnum; AExpValue: Byte);
begin
  if (Skill[ASkill].Value < SkillMax) then
  begin
    FSkill[ASkill].Exp := FSkill[ASkill].Exp + Math.RandomRange(0, AExpValue + 1) + 1;
    if (Skill[ASkill].Exp >= SkillExpMax) then
    begin
      FSkill[ASkill].Exp := FSkill[ASkill].Exp - SkillExpMax;
      Inc(FSkill[ASkill].Value);
      FSkill[ASkill].Value := EnsureRange(FSkill[ASkill].Value, SkillMin,
        SkillMax);
      // Add message {!!!}
      MsgLog.Add(Terminal.Colorize(Format('Your skill %s has raised to %d!',
        [GetName(ASkill), FSkill[ASkill].Value]), clAlarm));
      // Add exp
      Player.AddExp();
      // Add scores
      if (FSkill[ASkill].Value = SkillMax) then
        Player.Score := Player.Score + 50;
      Player.Calc;
    end;
  end;

end;

function TSkills.GetName(I: TSkillEnum): string;
begin
  case I of
    skLearning:
      Result := _('Learning');
    // Attributes skills
    skAthletics:
      Result := _('Athletics');
    skDodge:
      Result := _('Dodge');
    skConcentration:
      Result := _('Concentration');
    skToughness:
      Result := _('Toughness');
    // Weapon skills
    skBlade:
      Result := _('Blade');
    skAxe:
      Result := _('Axe');
    skSpear:
      Result := _('Spear');
    skMace:
      Result := _('Mace');
    // Skills
    skStealth:
      Result := _('Stealth');
    skHealing:
      Result := _('Healing');
  end;
end;

function TSkills.GetSkill(I: TSkillEnum): TSkill;
begin
  Result := FSkill[I]
end;

procedure TSkills.Modify(I: TSkillEnum; Value: Integer);
begin
  FSkill[I].Value := FSkill[I].Value + Value;
end;

procedure TSkills.SetSkill(I: TSkillEnum; const Value: TSkill);
begin
  FSkill[I] := Value
end;

procedure TSkills.Start;
var
  I: TSkillEnum;
begin
  // Wizard
  if not Game.Wizard then
    Exit;
  //
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    with FSkill[I] do
    begin
      Value := Math.RandomRange(SkillMin, SkillMax);
      Exp := Math.RandomRange(0, SkillExpMax);
    end;
end;

end.
