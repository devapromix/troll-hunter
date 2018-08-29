unit Trollhunter.Scene.Spellbook;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneSpellbook = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneSpells }

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Language,
  Trollhunter.Game,
  Trollhunter.Player.Spellbook,
  Trollhunter.Terminal,
  Trollhunter.Item,
  Trollhunter.Item.Helpers;

procedure TSceneSpellbook.Render;
var
  I: TSpellEnum;
  V: UInt;

  function IsSpell(I: TSpellEnum): Boolean;
  begin
    Result := Spellbook.GetSpell(I).Enable;
    if Mode.Wizard then
      Result := True;
  end;

begin
  inherited;
  UI.Title(_('Spellbook'));

  V := 0;
  Y := 2;
  UI.FromAToZ;
  for I := Low(TSpellEnum) to High(TSpellEnum) do
    if IsSpell(I) then
    begin
      Terminal.Print(1, Y, UI.KeyToStr(Chr(V + Ord('A'))));
      Terminal.ForegroundColor(clGray);
      Terminal.Print(5, Y, Format('(%s) %s %s', [Items.GetItemLevel(Spellbook.GetSpell(I).Spell.Level), Spellbook.GetSpellName(I),
        Items.GetInfo('-', Spellbook.GetSpell(I).Spell.ManaCost, 'Mana')]));
      Inc(Y);
      Inc(V);
    end;
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Cast spell'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneSpellbook.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_A .. TK_Z:
      Spellbook.DoSpell(Key - TK_A);
  end
end;

end.
