unit Trollhunter.Scene.Spellbook;

interface

uses
  Trollhunter.Types,
  Trollhunter.Spell,
  Trollhunter.Scenes;

type
  TSceneSpellbook = class(TScene)
  private
    FSelecting: boolean;
    procedure RenderSpellInfo(const ASpellEnum: TSpellEnum; const ASpell: TSpellData);
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Game,
  Trollhunter.Player,
  Trollhunter.Attribute,
  Trollhunter.Item,
  Trollhunter.Spell.School,
  Trollhunter.Spellbook,
  Trollhunter.Projectile.Types;

  { TSceneSpellbook }

procedure TSceneSpellbook.RenderSpellInfo(const ASpellEnum: TSpellEnum;
  const ASpell: TSpellData);
var
  LSpellSchool: TSpellSchoolData;
  LInfo, LSpellLevel, LDamage, LSpellIcon, LManaCost: string;
begin
  LSpellSchool := GetSpellSchoolData(ASpell.School);
  LSpellLevel := Game.IfThen(ASpell.Level > Player.Attributes.Attrib[atLev].Value,
    Items.GetLevel(ASpell.Level), '');
  LSpellIcon := Terminal.Colorize(UI.Icon(ASpell.Icon), LSpellSchool.Color);
  LDamage := '';
  if (ASpell.Projectile <> prNone) then
    LDamage := Format('%s%d-%d', [UI.Icon(icSword), Player.SpellMinDamage(ASpellEnum),
      Player.SpellMaxDamage(ASpellEnum)]);
  LManaCost := Items.GetInfo('-', ASpell.ManaCost, 'Mana');
  if (Player.Attributes.Attrib[atMana].Value < ASpell.ManaCost) then
    LManaCost := Terminal.Colorize(LManaCost, 'NoMana');
  LInfo := Items.AddItemInfo([LSpellLevel, LSpellIcon, LDamage, LManaCost]);
  Terminal.ForegroundColor(clGray);
  Terminal.Print(20, Y, LInfo);
  Terminal.Print(37, Y, Terminal.Colorize('{' + LSpellSchool.Name +
    '}', LSpellSchool.Color));
  Terminal.Print(50, Y, ASpell.Description);
end;

procedure TSceneSpellbook.Render;
var
  I: TSpellEnum;
  V: UInt;
  LSpell: TSpellData;
  IsActive, IsQuickSpell: boolean;
begin
  if FSelecting then
    UI.Title('Select Quick Spell')
  else if Spellbook.GetQuickSpell.Enable then
    UI.Title('Spellbook [[' + UI.Icon(icBook) + ' ' +
      Spellbook.GetQuickSpell.Spell.Name + ']]')
  else
    UI.Title('Spellbook');

  V := 0;
  Y := 2;
  UI.FromAToZ;

  for I := Low(TSpellEnum) to High(TSpellEnum) do
  begin
    if not Spellbook.GetSpell(I).Enable then
      Continue;

    if Mode.Wizard then
      IsActive := True
    else
      IsActive := Spellbook.GetSpell(I).Enable;
    if not IsActive then Continue;
    LSpell := GetSpellData(I);
    IsQuickSpell := Spellbook.GetQuickSpell.Enable and
      (Spellbook.GetQuickSpellEnum = I);
    Terminal.Print(1, Y, UI.KeyToStr(Chr(V + Ord('A')), '',
      Game.IfThen(IsQuickSpell, 'QuickSpell', 'Key')));
    if IsQuickSpell then
      Terminal.ForegroundColor(clLightYellow)
    else
      Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, LSpell.Name);
    RenderSpellInfo(I, LSpell);
    Inc(Y);
    Inc(V);
  end;

  if (V > 0) then
  begin
    MsgLog.Render(2, True);
  end;

  if FSelecting then
  begin
    AddKey('A-Z', 'Set Quick Spell');
    AddKey('Esc', 'Cancel', True);
  end
  else
  begin
    AddKey('A-Z', 'Cast Spell');
    AddKey('TAB', 'Set Quick Spell');
    AddKey('Backspace', 'Clear Quick Spell');
    AddKey('Esc', 'Close', True);
  end;
end;

procedure TSceneSpellbook.Update(var Key: UInt);
begin
  if FSelecting then
  begin
    case Key of
      TK_ESCAPE:
        FSelecting := False;
      TK_A .. TK_Z:
        if (Key - TK_A) < Spellbook.GetActiveSpellCount then
        begin
          Spellbook.SetQuickSpell(Spellbook.GetSpellByIndex(Key - TK_A));
          FSelecting := False;
        end;
    end;
    Exit;
  end;
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_A .. TK_Z:
    begin
      Spellbook.DoSpell(Key - TK_A);
      if Player.FireMode then
        Scenes.SetScene(scGame);
    end;
    TK_TAB:
      FSelecting := True;
    TK_BACKSPACE:
      Spellbook.ClearQuickSpell;
  end;
end;

end.
