unit uPlayer;

interface

type
  TSkillEnum = (skLearning,
    // Attributes skills
    skAthletics, skDodge, skConcentration, skToughness,
    // Weapon skills
    skSword, skAxe, skSpear, skDagger,
    // Skills
    skStealth, skHealing);

type
  TSkill = record
    Value: Integer;
    Exp: Integer;
  end;

const
  SkillMin = 5;
  SkillMax = 75;
  SkillExp = 100;
  AtrMax = 100;
  RadiusMax = 15;
  DVMax = 80;
  PVMax = 250;
  ExpMax = 10;

type
  TPlayer = class(TObject)
  private
    FX: Byte;
    FY: Byte;
    FLX: Byte;
    FLY: Byte;
    FTurn: Word;
    FLevel: Byte;
    FLife: Word;
    FMaxLife: Word;
    FMana: Word;
    FMaxMana: Word;
    FRadius: Byte;
    FDV: Byte;
    FPV: Byte;
    FDamage: Byte;
    FLook: Boolean;
    FStrength: Byte;
    FDexterity: Byte;
    FWillpower: Byte;
    FPerception: Byte;
    FSkill: array [TSkillEnum] of TSkill;
  public
    constructor Create;
    destructor Destroy; override;
    property X: Byte read FX write FX;
    property Y: Byte read FY write FY;
    property LX: Byte read FLX write FLX;
    property LY: Byte read FLY write FLY;
    property Turn: Word read FTurn write FTurn;
    property Level: Byte read FLevel write FLevel;
    property Life: Word read FLife write FLife;
    property MaxLife: Word read FMaxLife write FMaxLife;
    property Mana: Word read FMana write FMana;
    property MaxMana: Word read FMaxMana write FMaxMana;
    property Radius: Byte read FRadius write FRadius;
    property DV: Byte read FDV write FDV;
    property PV: Byte read FPV write FPV;
    property Look: Boolean read FLook write FLook;
    property Strength: Byte read FStrength write FStrength;
    property Dexterity: Byte read FDexterity write FDexterity;
    property Willpower: Byte read FWillpower write FWillpower;
    property Perception: Byte read FPerception write FPerception;
    procedure Render(AX, AY: Byte);
    procedure Move(AX, AY: ShortInt);
    property Damage: Byte read FDamage write FDamage;
    procedure Calc;
    procedure Fill;
    procedure Wait;
    procedure AddTurn;
    function GetRadius: Byte;
    function GetDV: Byte;
    function GetPV: Byte;
    function SaveCharacterDump(AReason: string): string;
    procedure Skill(ASkill: TSkillEnum; AExpValue: Byte = 10);
    function GetSkill(ASkill: TSkillEnum): TSkill;
    procedure Defeat(AKiller: string);
    procedure Attack(Index: Integer);
    function GetSkillName(ASkill: TSkillEnum): string;
  end;

var
  Player: TPlayer = nil;

implementation

uses Classes, SysUtils, Dialogs, Math, uCommon, uMap, uMob, uScenes,
  uTerminal, uMsgLog, gnugettext;

{ TPlayer }

procedure TPlayer.AddTurn;
begin
  Turn := Turn + 1;
  Mobs.Process;
end;

procedure TPlayer.Attack(Index: Integer);
var
  Mob: TMob;
  Dam: Word;
  The: string;
begin
  if (Index < 0) then
    Exit;
  Mob := Mobs.FMob[Index];
  if not Mob.Alive then
    Exit;
  The := GetDescThe(Mobs.GetName(TMobEnum(Mob.ID)));
  if (MobBase[TMobEnum(Mob.ID)].DV < Math.RandomRange(0, 100)) then
  begin
    // Attack
    Dam := Clamp(Self.Damage, 0, High(Word));
    Mob.Life := Clamp(Mob.Life - Dam, 0, High(Word));
    MsgLog.Add(Format(_('You hit %s (%d).'), [The, Dam]));
    if (Mob.Life = 0) then
      Mob.Defeat;
  end
  else
  begin
    // Miss
    MsgLog.Add(Format(_('You fail to hurt %s.'), [The]));
  end;
  AddTurn;
end;

procedure TPlayer.Calc;
begin
  Strength := Clamp(Round(FSkill[skAthletics].Value * 0.5) +
    Round(FSkill[skToughness].Value * 0.9), 1, AtrMax);
  Dexterity := Clamp(Round(FSkill[skDodge].Value * 1.4), 1, AtrMax);
  Willpower := Clamp(Round(FSkill[skConcentration].Value * 1.4), 1, AtrMax);
  Perception := Clamp(Round(FSkill[skToughness].Value * 1.4), 1, AtrMax);
  DV := Clamp(Round(Dexterity * (DVMax / AtrMax)), 0, DVMax);
  PV := Clamp(Round(FSkill[skToughness].Value / 1.4) - 4 { +ItemProp } ,
    0, PVMax);
  MaxLife := Round(Strength * 3.6) + Round(Dexterity * 2.3);
  MaxMana := Round(Willpower * 4.2) + Round(Dexterity * 0.4);
  Radius := Round(Perception / 8.3);
  Damage := Clamp(5, 1, High(Byte));
  Self.Fill;
end;

constructor TPlayer.Create;
var
  I: TSkillEnum;
begin
  Turn := 0;
  Level := 1;
  Look := False;
  for I := Low(TSkillEnum) to High(TSkillEnum) do
    with FSkill[I] do
    begin
      if WizardMode then
        Value := Math.RandomRange(SkillMin, SkillMax)
      else
        Value := SkillMin;
      Exp := Math.RandomRange(0, SkillExp);
    end;
  Self.Calc;
end;

procedure TPlayer.Defeat(AKiller: string);
begin
  Killer := AKiller;
  MsgLog.Add(_('You die...'));
  TextScreenshot := GetTextScreenshot();
end;

destructor TPlayer.Destroy;
begin

  inherited;
end;

procedure TPlayer.Fill;
begin
  Life := MaxLife;
  Mana := MaxMana;
end;

function TPlayer.GetDV: Byte;
begin
  Result := Clamp(Self.DV, 0, DVMax);
end;

function TPlayer.GetPV: Byte;
begin
  Result := Clamp(Self.PV, 0, PVMax);
end;

function TPlayer.GetRadius: Byte;
begin
  Result := Clamp(Self.Radius + 3, 1, RadiusMax);
end;

function TPlayer.GetSkill(ASkill: TSkillEnum): TSkill;
begin
  Result := FSkill[ASkill];
end;

function TPlayer.GetSkillName(ASkill: TSkillEnum): string;
begin
  case ASkill of
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
    skSword:
      Result := _('Swords');
    skAxe:
      Result := _('Axes');
    skSpear:
      Result := _('Spears');
    skDagger:
      Result := _('Daggers');
    // Skills
    skStealth:
      Result := _('Stealth');
    skHealing:
      Result := _('Healing');
  end;
end;

procedure TPlayer.Move(AX, AY: ShortInt);
var
  FX, FY: Byte;
begin
  if Look then
  begin
    if Map.InMap(LX + AX, LY + AY) and
      ((Map.InView(LX + AX, LY + AY) and not Map.GetFog(LX + AX, LY + AY)) or
      (WizardMode)) then
    begin
      LX := Clamp(LX + AX, 0, High(Byte));
      LY := Clamp(LY + AY, 0, High(Byte));
    end;
  end
  else
  begin
    if (Life = 0) then
    begin
      Scenes.SetScene(scDef);
      Exit;
    end;
    if WonGame then
    begin
      Scenes.SetScene(scWin);
      Exit;
    end;
    FX := Clamp(X + AX, 0, High(Byte));
    FY := Clamp(Y + AY, 0, High(Byte));
    if (Map.GetTileEnum(FX, FY, Map.Deep) in StopTiles) and not WizardMode then
      Exit;
    if not Mobs.GetFreeTile(FX, FY) then
    begin
      Self.Attack(Mobs.GetIndex(FX, FY));
    end
    else
    begin
      X := FX;
      Y := FY;
      AddTurn;
    end;
  end;
end;

procedure TPlayer.Render(AX, AY: Byte);
begin
  if (Self.Life = 0) then
    Terminal.Print(AX + View.Left, AY + View.Top, '%', clDarkGray)
  else
    Terminal.Print(AX + View.Left, AY + View.Top, '@', clDarkBlue);
end;

function TPlayer.SaveCharacterDump(AReason: string): string;
var
  SL: TStringList;
const
  F = '== %s ==';
begin
  if WizardMode then Exit;
  SL := TStringList.Create;
  try
    SL.Append(Format(F, [_('Trollhunter')]));
    SL.Append('');
    SL.Append(GetDateTime);
    SL.Append('');
    SL.Append(AReason);
    SL.Append('');
    SL.Append(Format(F, [_('Screenshot')]));
    SL.Append(TextScreenshot);
    SL.Append('');
    SL.Append(Format(F, [_('Last messages')]));
    SL.Append('');
    SL.Append(MsgLog.GetLastMsg(10));
    SL.Append('');
    SL.Append(Format(F, [_('Inventory')]));
    SL.Append('');
    SL.Append('');
    SL.SaveToFile(GetDateTime('-', '-') + '-character-dump.txt');
  finally
    SL.Free;
  end;
end;

procedure TPlayer.Skill(ASkill: TSkillEnum; AExpValue: Byte = 10);
begin
  if (FSkill[ASkill].Value < SkillMax) then
  begin
    Inc(FSkill[ASkill].Exp, AExpValue);
    if (FSkill[ASkill].Exp >= SkillExp) then
    begin
      FSkill[ASkill].Exp := FSkill[ASkill].Exp - SkillExp;
      Inc(FSkill[ASkill].Value);
      // Add message

      FSkill[ASkill].Value := Clamp(FSkill[ASkill].Value, SkillMin, SkillMax);
    end;
  end;
end;

procedure TPlayer.Wait;
begin
  if not DeepVis[Map.Deep] then
  begin
    MsgLog.Add(Format(_('You have opened a new territory: %s.'), [Map.GetName]));
    DeepVis[Map.Deep] := True;
  end;
  Move(0, 0);
end;

initialization

Player := TPlayer.Create;

finalization

Player.Free;
Player := nil;

end.
