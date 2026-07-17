unit Trollhunter.Scene.Store;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneStore = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Player,
  Trollhunter.Item,
  Trollhunter.Game;

procedure TSceneStore.Render;
begin
  UI.Title('Choose the item you wish to store', 1, clDarkestGreen);

  UI.FromAToZ;
  Items.RenderInventory;
  MsgLog.Render(2, True);

  AddKey('A-Z', 'Store an item');
  AddKey('Esc', 'Close', True);
end;

procedure TSceneStore.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.GoBack;
    TK_A .. TK_Z:
      Player.Drop(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

end.