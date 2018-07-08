unit Trollhunter.Scene.Load;

interface

uses Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneLoad = class(TScene)
  public
    procedure Render; override;
  end;

implementation

{ TSceneLoad }

uses BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Language;

procedure TSceneLoad.Render;
begin
  Terminal.Print(CX, CY, _('Creating the world, please wait...'),
    TK_ALIGN_CENTER);
end;

end.
