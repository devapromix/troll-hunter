unit Trollhunter.Scene.Spellbook;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes,
  Trollhunter.Creature;

type
  TSceneSpellbook = class(TScene)
  private
    FSelecting: boolean;
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Game,
  Trollhunter.Item,
  Trollhunter.Spellbook,
  Trollhunter.Magic;

{ TSceneSpellbook }

procedure TSceneSpellbook.Render;
var
  I: TSpellEnum;
  V: UInt;
  Spell: TSpellData;
  IsActive: boolean;
  LInfo: string;
  LQuickSpellName: string;
begin
  if Spellbook.GetQuickSpell.Enable then
    LQuickSpellName := Spellbook.GetQuickSpell.Spell.Name
  else
    LQuickSpellName := 'None';
  if FSelecting then
    UI.Title('Select Quick Spell')
  else
    UI.Title('Spellbook [[' + LQuickSpellName + ']]');

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
    Spell := SpellData[I];
    Terminal.Print(1, Y, UI.KeyToStr(Chr(V + Ord('A'))));
    Terminal.ForegroundColor(clWhite);
    Terminal.Print(5, Y, Spell.Name);
    LInfo := Format('[[Lev %d, %s',
      [Spell.Level, Items.GetInfo('-', Spell.ManaCost, 'Mana')]);
    LInfo := LInfo + ']]';
    Terminal.ForegroundColor(clGray);
    Terminal.Print(20, Y, LInfo);
    Terminal.Print(37, Y, Terminal.Colorize('{' + CSpellSchoolName[Spell.School] +
      '}', CSpellSchoolColor[Spell.School]));
    Terminal.Print(50, Y, Spell.Description);
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
      Spellbook.DoSpell(Key - TK_A);
    TK_TAB:
      FSelecting := True;
  end;
end;

end.
