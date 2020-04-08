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
  BearLibTerminal;

{ TSceneCredits }

procedure TSceneCredits.Render;
begin
  inherited;
  Y := 1;
  // Options
  Title(_('Credits'));
  Add(_('Author'), 'Sergiy Tkach (devapromix)');
  Add(_('Special thanks'), '');
  Add(_('Year'), '2018-2020');
  Add('', 'Vlad Phomin (Phomm)');
  Add;
  Add('', 'Eugene Loza (eugeneloza)');
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
