unit uItem;

interface

uses BearLibItems, uCommon, uMap, uPlayer;

type
  TItemType = (itCoin, itPotion, itSword);

type
  TItemBase = record
    Symbol: Char;
    Name: string;
    ItemType: TItemType;
    MaxStack: Word;
    MaxDurability: Word;
    Color: Cardinal;
    Deep: TDeepEnum;
  end;

  {$I Items.inc}

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

uses Math, uTerminal, gnugettext;

{ TItems }

procedure TItems.Add(ADeep: TDeepEnum);
var
  ID, FX, FY: Byte;
  FItem: Item;
  Value: Integer;
begin
  repeat
    ID := Math.RandomRange(0, Ord(High(TItemEnum)));
    FX := Math.RandomRange(0, High(Byte));
    FY := Math.RandomRange(0, High(Byte));
  until (Map.GetTileEnum(FX, FY, ADeep) in SpawnTiles) and
    ((ItemBase[TItemEnum(ID)].MaxStack > 1)
    or (ItemBase[TItemEnum(ID)].Deep = ADeep));
  FItem.MapID := Ord(ADeep);
  FItem.ItemID := Ord(ID);
  FItem.Amount := 1;
  FItem.X := FX;
  FItem.Y := FY;
  FItem.Stack := ItemBase[TItemEnum(ID)].MaxStack;
  FItem.Durability := ItemBase[TItemEnum(ID)].MaxDurability;
  case ItemBase[TItemEnum(ID)].ItemType of
    itCoin: FItem.Amount := (Math.RandomRange(0, 25) + 1) * (Ord(ADeep) + 1);
    itPotion: FItem.Amount := (Math.RandomRange(0, 3) + 1);
  end;
  if (FItem.Stack = 1) then
  begin
    Value := ItemBase[TItemEnum(ID)].MaxDurability;
    FItem.Durability := Math.RandomRange(Value div 4, Value) + 1;
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
      Color := ItemBase[TItemEnum(FItem.ItemID)].Color;
    Terminal.Print(X, Y, ItemBase[TItemEnum(FItem.ItemID)].Symbol, Color);
  end;
end;

initialization
  Items := TItems.Create;

finalization
  Items.Free;
  Items := nil;

end.
