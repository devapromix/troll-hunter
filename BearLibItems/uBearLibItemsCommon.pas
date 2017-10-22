unit uBearLibItemsCommon;

interface

type
  Item = record
    ItemID: Integer;
    X, Y: Integer;
    MapID: Integer;
    Identify: Integer;
    Stack: Integer;
    Amount: Integer;
    MinDamage: Integer;
    MaxDamage: Integer;
    Defense: Integer;
    Durability: Integer;
    MaxDurability: Integer;
    Weight: Integer;
    Size: Integer;
    SlotID: Integer;
    Equipment: Integer;
    Price: Integer;
    Attributes: Cardinal;
    Bonus: Cardinal;
    Color: Cardinal;
  end;

type
  TItems = array of Item;

const
  IntFalse = 0;
  IntTrue = 1;

// Library
procedure Items_Open(); stdcall;
procedure Items_Close(); stdcall;
function Items_GetVersion(): PWideChar; stdcall;

// Add
procedure Items_Clear_Item(var AItem: Item);

// Common
function HasItem(AItems: TItems; Index, AMapID: Integer): Boolean; overload;
function HasItem(AItems: TItems; Index, AMapID: Integer; AX, AY: Integer): Boolean; overload;
function IndexInRange(AItems: TItems; Index: Integer): Boolean;
procedure AddItem(var AItems: TItems; AItem: Item);
function DelItem(var AItems: TItems; Index: Integer): Item;
function HasEmpty(AItems: TItems): Boolean;
procedure Empty(var AItems: TItems);
function GlobalIndex(AItems: TItems; MapID, Index: Integer; AX: Integer = -1; AY: Integer = -1): Integer;

implementation

uses uBearLibItemsDungeon, uBearLibItemsInventory;

const
  LibVersion = '0.4.0';

// Library

procedure Items_Open(); stdcall;
begin
  Items_Dungeon_Clear();
  Items_Inventory_Clear();
end;

procedure Items_Close(); stdcall;
begin
  Items_Dungeon_Clear();
  Items_Inventory_Clear();
end;

function Items_GetVersion(): PWideChar; stdcall;
begin
  Result := LibVersion;
end;

// Add

procedure Items_Clear_Item(var AItem: Item);
begin
  with AItem do
  begin
    ItemID := -1;
    X := -1;
    Y := -1;
    MapID := -1;
    Identify := -1;
    Stack := 1;
    Amount := 1;
    MinDamage := 0;
    MaxDamage := 0;
    Defense := 0;
    Durability := 0;
    MaxDurability := 0;
    Weight := 1;
    Size := 1;
    SlotID := 0;
    Equipment := 0;
    Price := 0;
    Bonus := 0;
    Color := $FFFFFFFF;
  end;
end;

// Common

function HasItem(AItems: TItems; Index, AMapID: Integer): Boolean;
begin
  Result := (AItems[Index].MapID = AMapID);
end;

function HasItem(AItems: TItems; Index, AMapID: Integer; AX, AY: Integer): Boolean;
begin
  if (AX = -1) and (AY = -1) then
    Result := HasItem(AItems, Index, AMapID)
      else Result := (AItems[Index].MapID = AMapID) and (AItems[Index].X = AX) and (AItems[Index].Y = AY);
end;

function IndexInRange(AItems: TItems; Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Length(AItems));
end;

procedure AddItem(var AItems: TItems; AItem: Item);
begin
//  if (Length(AItems) <= Items_Inventory_GetSlotCount) then
//  begin
    SetLength(AItems, Length(AItems) + 1);
    AItems[Length(AItems) - 1] := AItem;
    {..}
//  end else begin

//  end;
end;

function DelItem(var AItems: TItems; Index: Integer): Item;
var
  I: Integer;
begin
  Result := AItems[Index];
  if (Length(AItems) > 1) then
    for I := Index to Length(AItems) - 2 do
      AItems[I] := AItems[I + 1];
  SetLength(AItems, Length(AItems) - 1);
end;

function HasEmpty(AItems: TItems): Boolean;
begin
  Result := (Length(AItems) = 0);
end;

procedure Empty(var AItems: TItems);
begin
  SetLength(AItems, 0);
end;

function GlobalIndex(AItems: TItems; MapID, Index: Integer; AX: Integer = -1; AY: Integer = -1): Integer;
var
  I, P: Integer;
begin
  Result := -1;
  if HasEmpty(AItems) then Exit;
  if not IndexInRange(AItems, Index)then Exit;
  P := 0;
  for I := 0 to Length(AItems) - 1 do
    if HasItem(AItems, I, MapID, AX, AY) then
    begin
      if (P = Index) then
      begin
        Result := I;
        Exit;
      end;
      Inc(P);
    end;
end;

end.
