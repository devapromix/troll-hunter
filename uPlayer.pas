unit uPlayer;

interface

type
  TSkillEnum = (
  skLearning,
  // Attributes skills
  skAthletics, skDodge, skConcentration, skToughness,
  //Weapon skills
  skSword, skAxe, skSpear, skDagger,
  // Skills
  skStealth, skHealing
  );

const
  SkillName: array [TSkillEnum] of string = (
  'Learning',
  'Athletics', 'Dodge', 'Concentration', 'Toughness',
  'Swords', 'Axes', 'Spears', 'Daggers',
  'Stealth', 'Healing'
  );

type
  TSkill = record
    Value: Integer;
    Exp: Integer;
  end;

const
  SkillMin  = 5;
  SkillMax  = 75;
  SkillExp  = 100;
  AtrMax    = 100;
  RadiusMax = 15;
  DVMax     = 80;
  PVMax     = 250;

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
    procedure Move(AX, AY: ShortInt);
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
  end;

var
  Player: TPlayer = nil;

implementation

uses Classes, SysUtils, Dialogs, Math, uCommon, uMap;

{ TPlayer }

procedure TPlayer.AddTurn;
begin
  Turn := Turn + 1;
end;

procedure TPlayer.Calc;
begin
  Strength := Clamp(Round(FSkill[skAthletics].Value * 0.5) +
    Round(FSkill[skToughness].Value * 0.9), 1, AtrMax);
  Dexterity := Clamp(Round(FSkill[skDodge].Value * 1.4), 1, AtrMax);
  Willpower := Clamp(Round(FSkill[skConcentration].Value * 1.4), 1, AtrMax);
  Perception := Clamp(Round(FSkill[skToughness].Value * 1.4), 1, AtrMax);
  DV := Clamp(Round(Dexterity * (DVMax / AtrMax)), 0, DVMax);
  PV := Clamp(Round(FSkill[skToughness].Value / 1.4) - 4{+ItemProp}, 0, PVMax);
  MaxLife := Round(Strength * 3.6) + Round(Dexterity * 2.3);
  MaxMana := Round(Willpower * 4.2) + Round(Dexterity * 0.4);
  Radius := Round(Perception / 8.3);
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
    Value := SkillMax; //Math.RandomRange(SkillMin, SkillMax);//SkillMin;
    Exp := Math.RandomRange(0, SkillExp);
  end;
  Self.Calc;
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

procedure TPlayer.Move(AX, AY: ShortInt);
var
  FX, FY: Byte;
begin
  if Look then
  begin
    if Map.InMap(LX + AX, LY + AY)
      and Map.InView(LX + AX, LY + AY)
      and not Map.GetFog(LX + AX, LY + AY) then
    begin
      LX := Clamp(LX + AX, 0, High(Byte));
      LY := Clamp(LY + AY, 0, High(Byte));
    end;
  end else begin
    FX := Clamp(X + AX, 0, High(Byte));
    FY := Clamp(Y + AY, 0, High(Byte));
    AddTurn;
    if (Map.GetTileEnum(FX, FY, Map.Deep) in StopTiles) and not WizardMode then Exit;
    X := FX;
    Y := FY;
  end;
end;

function TPlayer.SaveCharacterDump(AReason: string): string;
var
  I: Byte;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(CharacterDumpFileName);
    for I := 0 to SL.Count - 1 do
    begin
      SL[I] := StringReplace(SL[I], '{date-time}', GetDateTime, [rfReplaceAll]);
      SL[I] := StringReplace(SL[I], '{reason}', AReason, [rfReplaceAll]);
//      SL[I] := StringReplace(SL[I], '{screenshot}', , [rfReplaceAll]);
//      SL[I] := StringReplace(SL[I], '{messages}', , [rfReplaceAll]);
//      SL[I] := StringReplace(SL[I], '{inventory}', , [rfReplaceAll]);
    end;
    SL.SaveToFile(StringReplace(CharacterDumpFileName, 'trollhunter', GetDateTime('-', '-'), [rfReplaceAll]));
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
  Move(0, 0);
end;

initialization
  Player := TPlayer.Create;

finalization
  Player.Free;
  Player := nil;

end.
