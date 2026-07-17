unit Trollhunter.Scene.Items;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneItems = class(TScene)
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
  Trollhunter.Item.Common,
  Trollhunter.Item.Types,
  Trollhunter.Item.Dungeon,
  Trollhunter.Map,
  Trollhunter.Helpers,
  Trollhunter.Game;

procedure TSceneItems.Render;
var
  I, FCount, MapID: Int;
  FItem: Item;
begin
  UI.Title('Pick up an item');

  UI.FromAToZ;
  MapID := Ord(Map.Current);
  FCount := Items_Dungeon_GetMapCountXY(MapID, Player.X, Player.Y).InRange(ItemMax);

  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(MapID, I, Player.X, Player.Y);
    Items.RenderInvItem(5, 2, I, FItem);
  end;

  MsgLog.Render(2, True);

  AddKey('Esc', 'Close');
  AddKey('Space', 'Pick up all items');
  AddKey('A-Z', 'Pick up an item', True);

  if (FCount <= 0) then
    Scenes.SetScene(scGame);
end;

procedure TSceneItems.Update(var Key: UInt);
var
  I, FCount: Int;
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_SPACE:
    begin
      FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), Player.X,
        Player.Y).InRange(ItemMax);
      for I := 0 to FCount - 1 do
        Items.AddItemToInv;
    end;
    TK_A .. TK_Z:
      Items.AddItemToInv(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

end.
