unit uBearLibItemsDungeon;

interface

uses uBearLibItemsCommon;

procedure Items_Dungeon_Clear(); stdcall;
procedure Items_Dungeon_MapClear(MapID: Integer); stdcall;
procedure Items_Dungeon_MapClearXY(MapID: Integer; AX, AY: Integer); stdcall;

function Items_Dungeon_GetCount(): Integer; stdcall;
function Items_Dungeon_GetMapCount(MapID: Integer): Integer; stdcall;
function Items_Dungeon_GetMapCountXY(MapID: Integer; AX, AY: Integer): Integer; stdcall;

function Items_Dungeon_GetItemCount(ItemID: Integer): Integer; stdcall;
function Items_Dungeon_GetMapItemCount(MapID, ItemID: Integer): Integer; stdcall;
function Items_Dungeon_GetMapItemCountXY(MapID, ItemID: Integer; AX, AY: Integer): Integer; stdcall;

function Items_Dungeon_SetItem(Index: Integer; AItem: Item): Integer; stdcall;
function Items_Dungeon_GetItem(Index: Integer): Item; stdcall;

function Items_Dungeon_SetMapItem(MapID, Index: Integer; AItem: Item): Integer; stdcall;
function Items_Dungeon_GetMapItem(MapID, Index: Integer): Item; stdcall;

function Items_Dungeon_SetMapItemXY(MapID, Index: Integer; AX, AY: Integer; AItem: Item): Integer; stdcall;
function Items_Dungeon_GetMapItemXY(MapID, Index: Integer; AX, AY: Integer): Item; stdcall;

procedure Items_Dungeon_AppendItem(AItem: Item); stdcall;

function Items_Dungeon_DeleteItem(Index: Integer; var AItem: Item): Integer; stdcall;
function Items_Dungeon_DeleteMapItem(MapID: Integer; Index: Integer; var AItem: Item): Integer; stdcall;
function Items_Dungeon_DeleteMapItemXY(MapID: Integer; Index, AX, AY: Integer; var AItem: Item): Integer; stdcall;

function Items_Dungeon_GetMapItemAmountXY(MapID, ItemID, AX, AY: Integer): Integer; stdcall;

implementation

var
  MapItems: TItems;

procedure Items_Dungeon_Clear(); stdcall;
begin
  Empty(MapItems);
end;

procedure Items_Dungeon_MapClear(MapID: Integer); stdcall;
var
  I: Integer;
begin
  for I := 0 to Length(MapItems) - 1 do
    if HasItem(MapItems, I, MapID) then
      DelItem(MapItems, I);
end;

procedure Items_Dungeon_MapClearXY(MapID: Integer; AX, AY: Integer); stdcall;
var
  I: Integer;
begin
  if HasEmpty(MapItems) then Exit;
  for I := 0 to Length(MapItems) - 1 do
    if HasItem(MapItems, I, MapID, AX, AY) then
       DelItem(MapItems, I);
end;

function Items_Dungeon_GetCount(): Integer; stdcall;
begin
  Result := Length(MapItems);
end;

function Items_Dungeon_GetMapCount(MapID: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(MapItems) - 1 do
    if HasItem(MapItems, I, MapID) then
      Inc(Result);
end;

function Items_Dungeon_GetMapCountXY(MapID: Integer; AX, AY: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  if HasEmpty(MapItems) then Exit;
  for I := 0 to Length(MapItems) - 1 do
    if HasItem(MapItems, I, MapID, AX, AY) then
      Inc(Result);
end;

function Items_Dungeon_GetItemCount(ItemID: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(MapItems) - 1 do
    if (MapItems[I].ItemID = ItemID) then
      Inc(Result);
end;

function Items_Dungeon_GetMapItemCount(MapID, ItemID: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(MapItems) - 1 do
    if (MapItems[I].MapID = MapID) and (MapItems[I].ItemID = ItemID) then
      Inc(Result);
end;

function Items_Dungeon_GetMapItemCountXY(MapID, ItemID: Integer; AX, AY: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  if HasEmpty(MapItems) then Exit;
  for I := 0 to Length(MapItems) - 1 do
    if HasItem(MapItems, I, MapID, AX, AY)
      and (MapItems[I].ItemID = ItemID) then
        Inc(Result);
end;

function Items_Dungeon_SetItem(Index: Integer; AItem: Item): Integer; stdcall;
begin
  Result := IntFalse;
  if IndexInRange(MapItems, Index) then
  begin
    MapItems[Index] := AItem;
    Result := IntTrue;
  end;
end;

function Items_Dungeon_GetItem(Index: Integer): Item; stdcall;
begin
  Result := MapItems[Index];
end;

function Items_Dungeon_SetMapItem(MapID, Index: Integer; AItem: Item): Integer; stdcall;
var
  I: Integer;
begin
  Result := IntFalse;
  if HasEmpty(MapItems) then Exit;
  if not IndexInRange(MapItems, Index) then Exit;
  I := GlobalIndex(MapItems, MapID, Index);
  if (I < 0) then Exit;
  if (AItem.Amount <= 0) and (Items_Dungeon_DeleteItem(Index, AItem) = IntTrue) then Exit;
  Result := Items_Dungeon_SetItem(I, AItem);
end;

function Items_Dungeon_GetMapItem(MapID, Index: Integer): Item; stdcall;
var
  I: Integer;
begin
  Items_Clear_Item(Result);
  if HasEmpty(MapItems) then Exit;
  if not IndexInRange(MapItems, Index) then Exit;
  I := GlobalIndex(MapItems, MapID, Index);
  if (I < 0) then Exit;
  Result := Items_Dungeon_GetItem(I);
  if (Result.Amount < 0) then Exit;
end;

function Items_Dungeon_SetMapItemXY(MapID, Index: Integer; AX, AY: Integer; AItem: Item): Integer; stdcall;
var
  I: Integer;
begin
  Result := IntFalse;
  if HasEmpty(MapItems) then Exit;
  if not IndexInRange(MapItems, Index) then Exit;
  I := GlobalIndex(MapItems, MapID, Index, AX, AY);
  if (I < 0) then Exit;
  if (AItem.Amount <= 0) and (Items_Dungeon_DeleteMapItemXY(MapID, Index, AX, AY, AItem) = IntTrue) then Exit;
  Result := Items_Dungeon_SetItem(I, AItem);
end;

function Items_Dungeon_GetMapItemXY(MapID, Index: Integer; AX, AY: Integer): Item; stdcall;
var
  I: Integer;
begin
  Items_Clear_Item(Result);
  if HasEmpty(MapItems) then Exit;
  if not IndexInRange(MapItems, Index) then Exit;
  I := GlobalIndex(MapItems, MapID, Index, AX, AY);
  if (I < 0) then Exit;
  Result := Items_Dungeon_GetItem(I);
  if (Result.Amount < 0) then Exit;
end;

procedure Items_Dungeon_AppendItem(AItem: Item); stdcall;
var
  I, J, A: Integer;

  procedure Add(AItem: Item); stdcall;
  var
    A, J: Integer;
  begin
    A := AItem.Amount;
    while (A > 0) do
    begin
      J := AItem.Stack;
      if (A - J < 0) then J := A;
      Dec(A, J);
      AItem.Amount := J;
      AddItem(MapItems, AItem);
    end;
  end;

begin
  if (AItem.Stack > 1) then
  begin
    if (Items_Dungeon_GetMapItemCountXY(AItem.MapID, AItem.ItemID, AItem.X, AItem.Y) > 0) then
    begin
      A := AItem.Amount;
      if not HasEmpty(MapItems) then
        for I := 0 to Length(MapItems) - 1 do
          if HasItem(MapItems, I, AItem.MapID, AItem.X, AItem.Y)
            and (MapItems[I].ItemID = AItem.ItemID) then
          begin
            if (MapItems[I].Amount < AItem.Stack) then
            begin
              J := AItem.Stack - MapItems[I].Amount;
              if (A - J < 0) then J := A;
              Dec(A, J);
              Inc(MapItems[I].Amount, J);
            end;
          end;
      while (A > 0) do
      begin
        J := AItem.Stack;
        if (A - J < 0) then J := A;
        Dec(A, J);
        AItem.Amount := J;
        AddItem(MapItems, AItem);
      end;
    end else Add(AItem);
  end else Add(AItem);
end;

function Items_Dungeon_DeleteItem(Index: Integer; var AItem: Item): Integer; stdcall;
begin
  Result := IntFalse;
  if IndexInRange(MapItems, Index) then
  begin
    AItem := DelItem(MapItems, Index);
    Result := IntTrue;
  end;
end;

function Items_Dungeon_DeleteMapItem(MapID: Integer; Index: Integer; var AItem: Item): Integer; stdcall;
var
  I, P: Integer;
begin
  Result := IntFalse;
  if not IndexInRange(MapItems, Index)then Exit;
  P := 0;
  for I := 0 to Length(MapItems) - 1 do
    if HasItem(MapItems, I, MapID) then
    begin
      if (P = Index) then
      begin
        AItem := DelItem(MapItems, I);
        Result := IntTrue;
        Exit;
      end;
      Inc(P);
    end;
end;

function Items_Dungeon_DeleteMapItemXY(MapID: Integer; Index, AX, AY: Integer; var AItem: Item): Integer; stdcall;
var
  I, P: Integer;
begin
  Result := IntFalse;
  if not IndexInRange(MapItems, Index)then Exit;
  P := 0;
  for I := 0 to Length(MapItems) - 1 do
    if HasItem(MapItems, I, MapID, AX, AY) then
    begin
      if (P = Index) then
      begin
        AItem := DelItem(MapItems, I);
        Result := IntTrue;
        Exit;
      end;
      Inc(P);
    end;
end;

function Items_Dungeon_GetMapItemAmountXY(MapID, ItemID, AX, AY: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  if HasEmpty(MapItems) then Exit;
  for I := 0 to Length(MapItems) - 1 do
    if HasItem(MapItems, I, MapID, AX, AY)
      and (MapItems[I].ItemID = ItemID) then
        Inc(Result, MapItems[I].Amount);
end;

end.
