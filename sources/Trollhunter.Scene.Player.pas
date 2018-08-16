unit Trollhunter.Scene.Player;

interface

uses
  Trollhunter.Types,
  Trollhunter.Attribute,
  Trollhunter.Scenes;

type
  TScenePlayer = class(TScene)
  private
    D, W: UInt;
    FSkillCursorTop: ShortInt;
    FRenderInfo: Boolean;
    procedure RenderPlayer;
    procedure RenderInfo;
    procedure RenderSkills;
    procedure Add(const AStr, AIcons, ABarColor: string; const ACur, AMax: Int); overload;
    procedure Add(const AStr, AIcons, ABarColor: string; const ACur: TAttrib; AMax: Int); overload;
  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TScenePlayer }

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.UI,
  Trollhunter.Player,
  Trollhunter.Player.Helpers,
  Trollhunter.Game,
  Trollhunter.Language, Trollhunter.Player.Skills, Trollhunter.Creature;

constructor TScenePlayer.Create;
begin
  FRenderInfo := False;
  FSkillCursorTop := 0;
end;

procedure TScenePlayer.Render;
begin
  D := 2;
  Y := 0;
  X := Math.EnsureRange(Terminal.Window.Width div 4, 10, UIntMax);

  UI.Title(Player.FullName);

  if FRenderInfo then
    RenderInfo()
  else
    Self.RenderPlayer();
  Self.RenderSkills();

  AddKey('Esc', _('Close'));
  AddKey('?', _('Help'), True);
end;

procedure TScenePlayer.Add(const AStr, AIcons, ABarColor: string; const ACur, AMax: Int);
begin
  W := X * 2 - 3;
  UI.Bar(1, 0, Y + (D * 2), W, ACur, AMax, color_from_name(LowerCase(ABarColor)), clDarkGray);
  Terminal.Print(X, Y + (D * 2), Format('%s %d/%d', [AIcons + ' ' + AStr, ACur, AMax]), TK_ALIGN_CENTER);
  Inc(D);
end;

procedure TScenePlayer.Add(const AStr, AIcons, ABarColor: string; const ACur: TAttrib; AMax: Int);
begin
  W := X * 2 - 3;
  UI.Bar(1, 0, Y + (D * 2), W, ACur.Value, AMax, color_from_name(LowerCase(ABarColor)), clDarkGray);
  if Mode.Wizard then
    Terminal.Print(X, Y + (D * 2), Format('%s %d(%d)/%d', [AIcons + ' ' + AStr, ACur.Value, ACur.Prm, AMax]), TK_ALIGN_CENTER)
  else
    Terminal.Print(X, Y + (D * 2), Format('%s %d/%d', [AIcons + ' ' + AStr, ACur.Value, AMax]), TK_ALIGN_CENTER);
  Inc(D);
end;

procedure TScenePlayer.RenderPlayer;
begin
  Terminal.Print(X, Y + 2, Format(FT, [_('Attributes') + ' (1/2)']), TK_ALIGN_CENTER);
  // Level
  Add(Format('%s %d', [_('Level'), Player.Attributes.Attrib[atLev].Value]), UI.Icon(icElixir), 'Gold', Player.Attributes.Attrib[atExp].Value,
    Player.Attributes.Attrib[atMaxExp].Value);
  // Attributes
  Add('Strength', UI.Icon(icStr), 'Strength', Player.Attributes.Attrib[atStr], AttribMax);
  Add('Dexterity', UI.Icon(icDex), 'Dexterity', Player.Attributes.Attrib[atDex], AttribMax);
  Add('Willpower', UI.Icon(icBook), 'Willpower', Player.Attributes.Attrib[atWil], AttribMax);
  Add('Perception', UI.Icon(icLeaf), 'Perception', Player.Attributes.Attrib[atPer], AttribMax);
  // Damage
  Add('Min Damage', UI.Icon(icSword), 'Darker Yellow', Player.Attributes.Attrib[atMinDamage].Value, MinDamMax);
  Add('Max Damage', UI.Icon(icSword), 'Darker Yellow', Player.Attributes.Attrib[atMaxDamage].Value, MaxDamMax);
  // DV and PV
  Add('Defensive Value (DV)', UI.Icon(icDex), 'Darkest Green', Player.Attributes.Attrib[atDV].Value, DVMax);
  Add('Protection Value (PV)', UI.Icon(icShield), 'Darkest Green', Player.Attributes.Attrib[atPV].Value, PVMax);
  // Life and Mana
  Add('Life', UI.Icon(icLife), 'Life', Player.Attributes.Attrib[atLife], Player.Attributes.Attrib[atMaxLife].Value);
  Add('Mana', UI.Icon(icMana), 'Mana', Player.Attributes.Attrib[atMana], Player.Attributes.Attrib[atMaxMana].Value);
  // Vision radius
  Add('Vision radius', UI.Icon(icVision), 'Vision', Player.Vision, VisionMax);
end;

procedure TScenePlayer.RenderInfo;
begin
  Terminal.Print(X, Y + 2, Format(FT, [_('Attributes') + ' (2/2)']), TK_ALIGN_CENTER);
  //
  Add('Replenish Life', UI.Icon(icElixir) + UI.Icon(icLife), 'Life', Player.Attributes.Attrib[atReLife].Value, ReLifeMax);
  Add('Regeneration Mana', UI.Icon(icElixir) + UI.Icon(icMana), 'Mana', Player.Attributes.Attrib[atReMana].Value, ReManaMax);
  //
  Add('To Life after each Kill', UI.Icon(icPlus) + UI.Icon(icLife), 'Life', Player.Attributes.Attrib[atLifeAfEachKill].Value, LifeAEKMax);
  Add('To Mana after each Kill', UI.Icon(icPlus) + UI.Icon(icLife), 'Mana', Player.Attributes.Attrib[atManaAfEachKill].Value, ManaAEKMax);
  //
  Add('Extra Gold from Monsters (%)', UI.Icon(icPlus) + UI.Icon(icGold), 'Gold', Player.Attributes.Attrib[atExtraGold].Value, ExtraGoldMax);
  //
  Add('Satiation', UI.Icon(icFood), 'Food', Player.Attributes.Attrib[atSat].Value, EngorgedMax);
end;

const
  ScrMax = 12;

procedure TScenePlayer.RenderSkills;
var
  I: TSkillEnum;
  A, B, J, D: UInt;
begin
  Y := 2;
  X := Terminal.Window.Width div 2;
  A := Terminal.Window.Width div 4;
  B := A * 3;
  Terminal.Print(B, Y, Format(FT, [Format(_('Skills (%d-%d)'), [FSkillCursorTop + 1, FSkillCursorTop + ScrMax])]), TK_ALIGN_CENTER);
  for J := 1 to ScrMax do
  begin
    I := TSkillEnum(FSkillCursorTop + J);
    D := ((J - 1) * 2) + Y + 2;
    UI.Bar(X, 0, D, X - 2, Player.Skills.Skill[I].Value, SkillMax, clDarkRed, clDarkGray);
    Terminal.Print(B, D, Format('%s %d/%d', [Player.Skills.GetName(I), Player.Skills.Skill[I].Value, SkillMax]), TK_ALIGN_CENTER);
  end;
end;

procedure TScenePlayer.Update(var Key: UInt);
begin
  case Key of
    // Close
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    // Background
    TK_TAB:
      Scenes.SetScene(scBackground, scPlayer);
    // Information
    TK_LEFT, TK_A, TK_KP_4:
      FRenderInfo := False;
    TK_RIGHT, TK_D, TK_KP_6:
      FRenderInfo := True;
    // Inventory
    TK_SPACE:
      begin
        Game.Timer := UIntMax;
        Scenes.SetScene(scInv);
      end;
    TK_UP, TK_KP_8, TK_W:
      begin
        if (FSkillCursorTop > 0) then
          Dec(FSkillCursorTop);
      end;
    TK_DOWN, TK_KP_2, TK_X:
      begin
        if (FSkillCursorTop < Ord(High(TSkillEnum)) - ScrMax) then
          Inc(FSkillCursorTop);
      end;
    TK_SLASH:
      Scenes.SetScene(scHelp, scPlayer);
  end;
end;

end.
