unit Trollhunter.Scene.Identification;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneIdentification = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneIdentification }

uses
  BearLibTerminal,
  Trollhunter.Player,
  Trollhunter.Game,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Item,
  Trollhunter.Language;

procedure TSceneIdentification.Render;
var
  C: UInt;
begin
  UI.Title(_('Identification'), 1, clDarkestRed);

  UI.FromAToZ();
  C := Items.RenderInventory();
  MsgLog.Render(2, True);

  AddKey(C, _('Select an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneIdentification.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scInv);
    TK_A .. TK_Z:
      Player.IdentItem(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end
end;

end.
