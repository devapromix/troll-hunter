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

uses
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.Player,
  Trollhunter.Item,
  Trollhunter.Item.Common,
  Trollhunter.Item.Dungeon,
  Trollhunter.Map,
  Trollhunter.Helpers,
  Trollhunter.Game;

procedure TSceneStash.Render;
var
  I, FCount, MapID: Int;
  FItem: Item;
begin
  UI.Title('Stash', 1, clDarkestGreen);

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
  AddKey('Space', 'Show Inventory');
  AddKey('A-Z', 'Take an item', True);
end;

procedure TSceneStash.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      Scenes.SetScene(scGame);
    TK_SPACE:
    begin
      Game.Timer := UIntMax;
      Scenes.SetScene(scInv);
    end;
    TK_A .. TK_Z:
      Items.AddItemToInv(Key - TK_A);
    else
      Game.Timer := UIntMax;
  end;
end;

end.
