unit Trollhunter.Scene.Messages;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneMessages = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneMessages }

uses
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Language;

procedure TSceneMessages.Render;
begin
  inherited;
  UI.Title(_('Last messages'));
  MsgLog.RenderAllMessages;
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneMessages.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
  end;
end;

end.
