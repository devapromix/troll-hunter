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

{ TSceneItems }

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
  Trollhunter.Game;

procedure TSceneItems.Render;
var
  I, FCount, MapID: Int;
  FItem: Item;
begin
  MapID := Ord(Map.Current);

  if Player.IsOnStash then
    UI.Title('Stash', 1, clDarkestGreen)
  else
    UI.Title('Pick up an item');

  UI.FromAToZ;
  FCount := Items_Dungeon_GetMapCountXY(MapID, Player.X, Player.Y).InRange(ItemMax);

  for I := 0 to FCount - 1 do
  begin
    FItem := Items_Dungeon_GetMapItemXY(MapID, I, Player.X, Player.Y);
    Items.RenderInvItem(5, 2, I, FItem);
  end;

  MsgLog.Render(2, True);

  if Player.IsOnStash then
  begin
    AddKey('Esc', 'Close');
    AddKey('Space', 'Take all items from stash');
    AddKey('A-Z', 'Take an item', True);
  end
  else
  begin
    AddKey('Esc', 'Close');
    AddKey('Space', 'Pick up all items');
    AddKey('A-Z', 'Pick up an item', True);
  end;

  if (FCount <= 0) then
    Scenes.SetScene(scGame);
end;

procedure TSceneItems.Update(var Key: UInt);
var
  I, FCount: Int;
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_SPACE:
    begin
      FCount := Items_Dungeon_GetMapCountXY(Ord(Map.Current), Player.X,
        Player.Y).InRange(ItemMax);
      for I := 0 to FCount - 1 do
        Items.AddItemToInv;
    end;
    TK_A .. TK_Z:
      // Pick up
      Items.AddItemToInv(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

end.
