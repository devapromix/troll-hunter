unit uItem;

interface

uses BearLibItems, uCommon, uMap, uPlayer;

type
  TItemBase = record
    Symbol: Char;
    Name: string;
    MaxDurability: Byte;
    Color: Cardinal;
    Deep: TDeepEnum;
  end;

const
  ItemCount = 6;
  //
  itGold = 0;

const
  ItemBase: array [0 .. ItemCount - 1] of TItemBase = (
    // All
    (Symbol: '$'; Name: 'Gold'; MaxDurability: 0; Color: clYellow;
    Deep: deDarkWood;),
    // Dark Wood
    (Symbol: '/'; Name: 'Item'; MaxDurability: 10; Color: clYellow;
    Deep: deDarkWood;),
    // Gray Cave
    (Symbol: '\'; Name: 'Item'; MaxDurability: 10; Color: clYellow;
    Deep: deGrayCave;),
    // Deep Cave
    (Symbol: '['; Name: 'Item'; MaxDurability: 10; Color: clYellow;
    Deep: deDeepCave;),
    // Blood Cave
    (Symbol: '^'; Name: 'Item'; MaxDurability: 10; Color: clYellow;
    Deep: deBloodCave;),
    // Dungeon of Doom
    (Symbol: '{'; Name: 'Item'; MaxDurability: 10; Color: clYellow;
    Deep: deDungeonOfDoom;));

type
  TItems = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(AX, AY: Byte);
    procedure Add(ADeep: TDeepEnum);
  end;

var
  Items: TItems = nil;

implementation

uses Math, uTerminal;

{ TItems }

procedure TItems.Add(ADeep: TDeepEnum);
var
  ID, FX, FY: Byte;
  FItem: Item;
  D: Integer;
begin
  repeat
    ID := Math.RandomRange(0, ItemCount);
    FX := Math.RandomRange(0, High(Byte));
    FY := Math.RandomRange(0, High(Byte));
  until (Map.GetTileEnum(FX, FY, ADeep) in SpawnTiles) and
    ((ID <= itGold) or (ItemBase[ID].Deep = ADeep));
  FItem.MapID := Ord(ADeep);
  FItem.ItemID := ID;
  FItem.X := FX;
  FItem.Y := FY;
  case FItem.ItemID of
    itGold: // Gold
      begin
        FItem.Stack := 1000;
        FItem.Amount := (Math.RandomRange(0, 25) + 1) * Ord(ADeep);
      end;
  else
    begin
      FItem.Stack := 1;
      FItem.Amount := 1;
    end;
  end;
  if (FItem.Stack = 1) then
  begin
    D := ItemBase[ID].MaxDurability;
    FItem.Durability := Math.RandomRange(D div 5, D) + 1;
  end;
  Items_Dungeon_AppendItem(FItem);
end;

constructor TItems.Create;
begin
  Items_Open;
end;

destructor TItems.Destroy;
begin
  Items_Close;
  inherited;
end;

procedure TItems.Render(AX, AY: Byte);
var
  MapID, X, Y: Byte;
  I, Count: Integer;
  Color: Cardinal;
  FItem: Item;
begin
  MapID := Ord(Map.Deep);
  Count := Items_Dungeon_GetMapCount(MapID);
  for I := Count - 1 downto 0 do
  begin
    FItem := Items_Dungeon_GetMapItem(MapID, I);
    if not Map.InView(FItem.X, FItem.Y) or
      (not WizardMode and not Map.GetFOV(FItem.X, FItem.Y)) then
      Continue;
    X := FItem.X - Player.X + AX + View.Left;
    Y := FItem.Y - Player.Y + AY + View.Top;
    if not WizardMode and (GetDist(Player.X, Player.Y, FItem.X, FItem.Y) >
      Player.GetRadius) then
      Color := clFog
    else
      Color := ItemBase[FItem.ItemID].Color;
    Terminal.Print(X, Y, ItemBase[FItem.ItemID].Symbol, Color);
  end;
end;

initialization

Items := TItems.Create;

finalization

Items.Free;
Items := nil;

end.
