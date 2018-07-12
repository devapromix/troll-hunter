unit Trollhunter.Scene.Load;

interface

uses Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneLoad = class(TScene)
  public
    IsLoad: Boolean;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneLoad }

uses BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Language;

procedure TSceneLoad.Render;
begin
  if IsLoad then
    Terminal.Print(CX, CY, _('Loading...'), TK_ALIGN_CENTER)
  else
    Terminal.Print(CX, CY, _('Creating the world, please wait...'),
      TK_ALIGN_CENTER);
end;

procedure TSceneLoad.Update(var Key: UInt);
begin

end;

end.
