unit Trollhunter.Scene.Item.Drop;

interface

uses Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneDrop = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneDrop }

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Terminal,
  Trollhunter.Item,
  Trollhunter.Game,
  Trollhunter.Language,
  Trollhunter.Player;

procedure TSceneDrop.Render;
begin
  UI.Title(_('Choose the item you wish to drop'), 1, clDarkestRed);

  UI.FromAToZ;
  Items.RenderInventory;
  MsgLog.Render(2, True);

  AddKey('A-Z', _('Drop an item'));
  AddKey('Esc', _('Close'), True);
end;

procedure TSceneDrop.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.GoBack;
    TK_A .. TK_Z: // Drop an item
      Player.Drop(Key - TK_A);
  else
    Game.Timer := UIntMax;
  end;
end;

end.
