unit Trollhunter.Scene.Rest;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneRest = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneRest }

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.Terminal,
  Trollhunter.Language,
  Trollhunter.UI.Log,
  Trollhunter.Player;

procedure TSceneRest.Render;
begin
  UI.Title(_('Rest duration'));

  UI.FromAToZ;
  Y := 1;

  Inc(Y);
  Terminal.Print(1, Y, UI.KeyToStr(Chr(Y + 95)) + ' ' + _('Rest for 10 turns'), TK_ALIGN_LEFT);
  Inc(Y);
  Terminal.Print(1, Y, UI.KeyToStr(Chr(Y + 95)) + ' ' + _('Rest for 100 turns'), TK_ALIGN_LEFT);
  Inc(Y);
  Terminal.Print(1, Y, UI.KeyToStr(Chr(Y + 95)) + ' ' + _('Rest for 1000 turns'), TK_ALIGN_LEFT);

  MsgLog.Render(2, True);

  AddKey('A-C', _('Rest'));
  AddKey('Esc', _('Back'), True);
end;

procedure TSceneRest.Update(var Key: UInt);
begin
  case Key of
    TK_A, TK_B, TK_C:
      Player.Rest(StrToInt('1' + StringOfChar('0', Key - TK_A + 1)));
    TK_ESCAPE:
      Scenes.SetScene(scGame);
  end
end;

end.
