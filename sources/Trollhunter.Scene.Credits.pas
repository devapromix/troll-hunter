unit Trollhunter.Scene.Credits;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneCredits = class(TScene)
  private

  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  SysUtils,
  Trollhunter.UI,
  Trollhunter.Language,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Player.Classes;

{ TSceneCredits }

procedure TSceneCredits.Render;
var
  I: UInt;
begin
  inherited;
  Y := 1;
  // Options
  Title(_('Credits'));
  Add;
  Add(_('Author'), 'Sergiy Tkach (Apromix)');
  Add;
  Add;
  Add;
  Add(_('Special thanks'), '');
  Add;
  Add('', 'Vlad Fomin (Phomm)');
  Add;
  Add('', 'Eugene Loza (Eugeneloza)');

  for I := 0 to 14 do
    Terminal.Print(10, I + 6, '[color=light gray]' + ClassPict[clWarrior][I]);

  AddKey('Esc', _('Back'), True);
end;

procedure TSceneCredits.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scTitle);
  end;
end;

end.
