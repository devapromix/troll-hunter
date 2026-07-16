unit Trollhunter.Scene.Stash;

interface

uses
  Trollhunter.Scenes,
  Trollhunter.Types;

type
  TSceneStash = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneStash }

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

procedure TSceneStash.Render;
var
  I, FCount, MapID: Int;
  FItem: Item;
begin
  MapID := Ord(Map.Current);

  if Player.IsOnStash then
    UI.Title('Stash');

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
    AddKey('Space', 'Show Inventory');
    AddKey('A-Z', 'Take an item', True);
  end;
end;

procedure TSceneStash.Update(var Key: UInt);
var
  I, FCount: Int;
begin
  case Key of
    TK_ESCAPE: // Close
      Scenes.SetScene(scGame);
    TK_SPACE:  // Inventory
    begin
      Game.Timer := UIntMax;
      Scenes.SetScene(scInv);
    end;
    TK_A .. TK_Z:// Take an item
      Items.AddItemToInv(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

end.
