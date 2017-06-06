unit BeaRLibItems;

interface

type
  Item = record
    ItemID: Integer;    
    X: Integer;
    Y: Integer;
    MapID: Integer;
    Stack: Integer;
    Amount: Integer;
    MinDamage: Integer;
    MaxDamage: Integer;
    Durability: Integer;
    Weight: Integer;
    Size: Integer;
    SlotID: Integer;
    Equipment: Integer;
  end;

// Library
procedure Items_Open(); stdcall; external 'BeaRLibItems.dll';
procedure Items_Close(); stdcall; external 'BeaRLibItems.dll';
function Items_GetVersion(): PChar; stdcall; external 'BeaRLibItems.dll';
procedure Items_Clear_Item(var AItem: Item); external 'BeaRLibItems.dll';

// Dungeon
procedure Items_Dungeon_Clear(); stdcall; external 'BeaRLibItems.dll';
procedure Items_Dungeon_MapClear(MapID: Integer); stdcall; external 'BeaRLibItems.dll';
procedure Items_Dungeon_MapClearXY(MapID: Integer; AX, AY: Integer); stdcall; external 'BeaRLibItems.dll';

function Items_Dungeon_GetCount(): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_GetMapCount(MapID: Integer): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_GetMapCountXY(MapID: Integer; AX, AY: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Dungeon_GetItemCount(ItemID: Integer): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_GetMapItemCount(MapID, ItemID: Integer): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_GetMapItemCountXY(MapID, ItemID: Integer; AX, AY: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Dungeon_SetItem(Index: Integer; AItem: Item): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_GetItem(Index: Integer): Item; stdcall; external 'BeaRLibItems.dll';

function Items_Dungeon_SetMapItem(MapID, Index: Integer; AItem: Item): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_GetMapItem(MapID, Index: Integer): Item; stdcall; external 'BeaRLibItems.dll';

function Items_Dungeon_SetMapItemXY(MapID, Index: Integer; AX, AY: Integer; AItem: Item): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_GetMapItemXY(MapID, Index: Integer; AX, AY: Integer): Item; stdcall; external 'BeaRLibItems.dll';

procedure Items_Dungeon_AppendItem(AItem: Item); stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_DeleteItem(Index: Integer; var AItem: Item): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Dungeon_DeleteItemXY(MapID: Integer; Index, AX, AY: Integer; var AItem: Item): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Dungeon_GetMapItemAmountXY(MapID, ItemID, AX, AY: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

// Inventory
procedure Items_Inventory_Clear(); stdcall; external 'BeaRLibItems.dll';

function Items_Inventory_GetCount(): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_GetItemCount(ItemID: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Inventory_GetWeight(): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_GetItemWeight(ItemID: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Inventory_GetSize(): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_GetItemSize(ItemID: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Inventory_GetItemAmount(ItemID: Integer): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_DeleteItemAmount(ItemID, Amount: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Inventory_SetItem(Index: Integer; AItem: Item): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_GetItem(Index: Integer): Item; stdcall; external 'BeaRLibItems.dll';

procedure Items_Inventory_AppendItem(AItem: Item); stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_DeleteItem(Index: Integer; var AItem: Item): Integer; stdcall; external 'BeaRLibItems.dll';

function Items_Inventory_EquipItem(Index: Integer): Integer; stdcall; external 'BeaRLibItems.dll';
function Items_Inventory_UnEquipItem(Index: Integer): Integer; stdcall; external 'BeaRLibItems.dll';

implementation

end.
