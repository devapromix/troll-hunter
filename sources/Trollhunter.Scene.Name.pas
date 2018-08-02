unit Trollhunter.Scene.Name;

interface

uses Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneName = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.UI,
  Trollhunter.Player,
  Trollhunter.Player.Helpers,
  Trollhunter.Game,
  Trollhunter.Language;

{ TSceneName }

procedure TSceneName.Render;
begin
  UI.Title(_('Choose a name'));

  Terminal.Print(CX - 14, CY - 1, _('Enter your player''s name') + ': ' +
    Player.Name + Game.GetCursor, TK_ALIGN_LEFT);
  Terminal.ForegroundColor(clGray);
  Terminal.Print(CX, CY + 1, Player.FullName, TK_ALIGN_CENTER);

  AddKey('Enter', _('Confirm'));
  AddKey('Esc', _('Back'), True);
end;

procedure TSceneName.Update(var Key: UInt);
begin
  case Key of
    TK_BACKSPACE:
      begin
        if (Player.Name <> '') then
          Player.Name := Copy(Player.Name, 1, Length(Player.Name) - 1);
      end;
    TK_ENTER, TK_KP_ENTER:
      begin
        if (Player.Name = '') then
          Player.Name := _('PLAYER');
        Scenes.SetScene(scBackground, scName);
      end;
    TK_A .. TK_Z:
      begin
        if (Length(Player.Name) < 10) then
          Player.Name := Player.Name + Chr(Key - TK_A + 65);
      end;
    TK_ESCAPE:
      begin
        Scenes.SetScene(scClass, scName);
      end;
  end;
end;

end.
