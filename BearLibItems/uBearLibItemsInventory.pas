unit uBearLibItemsInventory;

interface

uses uBearLibItemsCommon;

procedure Items_Inventory_Clear(); stdcall;

function Items_Inventory_GetCount(): Integer; stdcall;
function Items_Inventory_GetItemCount(ItemID: Integer): Integer; stdcall;

function Items_Inventory_GetWeight(): Integer; stdcall;
function Items_Inventory_GetItemWeight(ItemID: Integer): Integer; stdcall;

function Items_Inventory_GetSize(): Integer; stdcall;
function Items_Inventory_GetItemSize(ItemID: Integer): Integer; stdcall;

function Items_Inventory_GetItemAmount(ItemID: Integer): Integer; stdcall;
function Items_Inventory_DeleteItemAmount(ItemID, Amount: Integer): Integer; stdcall;

function Items_Inventory_SetItem(Index: Integer; AItem: Item): Integer; stdcall;
function Items_Inventory_GetItem(Index: Integer): Item; stdcall;

procedure Items_Inventory_AppendItem(AItem: Item); stdcall;
function Items_Inventory_DeleteItem(Index: Integer; var AItem: Item): Integer; stdcall;

function Items_Inventory_EquipItem(Index: Integer): Integer; stdcall;
function Items_Inventory_UnEquipItem(Index: Integer): Integer; stdcall;

procedure Items_Inventory_SetSlotCount(ACount: Integer); stdcall;
function Items_Inventory_GetSlotCount: Integer; stdcall;

implementation

var
  InvItems: TItems;
  SlotMax: Integer = 26;

procedure Items_Inventory_Clear(); stdcall;
begin
  Empty(InvItems);
end;

function Items_Inventory_GetCount(): Integer; stdcall;
begin
  Result := Length(InvItems);
end;

function Items_Inventory_GetItemCount(ItemID: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(InvItems) - 1 do
    if (InvItems[I].ItemID = ItemID) then
      Inc(Result);
end;

function Items_Inventory_GetWeight(): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(InvItems) - 1 do
    Inc(Result, InvItems[I].Weight);
end;

function Items_Inventory_GetItemWeight(ItemID: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(InvItems) - 1 do
    if (InvItems[I].ItemID = ItemID) then
      Inc(Result, InvItems[I].Weight);
end;

function Items_Inventory_GetSize(): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(InvItems) - 1 do
    Inc(Result, InvItems[I].Size);
end;

function Items_Inventory_GetItemSize(ItemID: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(InvItems) - 1 do
    if (InvItems[I].ItemID = ItemID) then
      Inc(Result, InvItems[I].Size);
end;

function Items_Inventory_GetItemAmount(ItemID: Integer): Integer; stdcall;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(InvItems) - 1 do
    if (InvItems[I].ItemID = ItemID) then
      Inc(Result, InvItems[I].Amount);
end;

function Items_Inventory_DeleteItemAmount(ItemID, Amount: Integer): Integer; stdcall;
var
  I, C: Integer;
  FItem: Item;
begin
  Result := IntFalse;
  if (Amount <= 0) or (Items_Inventory_GetItemAmount(ItemID) < Amount) then Exit;
  C := Amount;
  for I := 0 to Length(InvItems) - 1 do
    if (InvItems[I].ItemID = ItemID) then
    begin
      Result := IntTrue;
      if (InvItems[I].Amount >= C) then
      begin
        InvItems[I].Amount := InvItems[I].Amount - C;
        if (InvItems[I].Amount <= 0) then
        begin
          Items_Inventory_DeleteItem(I, FItem);
          Exit;
        end;
      end else begin
        C := C - InvItems[I].Amount;
        Items_Inventory_DeleteItem(I, FItem);
      end;
    end;
end;

function Items_Inventory_GetItem(Index: Integer): Item; stdcall;
begin
  if IndexInRange(InvItems, Index) then
    Result := InvItems[Index];
end;

function Items_Inventory_SetItem(Index: Integer; AItem: Item): Integer; stdcall;
begin
  Result := IntFalse;
  if (AItem.Amount <= 0) then
  begin
    Result := Items_Inventory_DeleteItem(Index, AItem);
    Exit;
  end;
  if IndexInRange(InvItems, Index) then
  begin
    InvItems[Index] := AItem;
    Result := IntTrue;
  end;
end;

procedure Items_Inventory_AppendItem(AItem: Item); stdcall;
var
  I, J, A: Integer;
begin
  if (AItem.Stack > 1) then
  begin
    if (Items_Inventory_GetItemCount(AItem.ItemID) > 0) then
    begin
      A := AItem.Amount;
      if not HasEmpty(InvItems) then
        for I := 0 to Length(InvItems) - 1 do
          if (InvItems[I].ItemID = AItem.ItemID) then
          begin
            if (InvItems[I].Amount < AItem.Stack) then
            begin
              J := AItem.Stack - InvItems[I].Amount;
              if (A - J < 0) then J := A;
              Dec(A, J);
              Inc(InvItems[I].Amount, J);
            end;
          end;
      while (A > 0) do
      begin
        J := AItem.Stack;
        if (A - J < 0) then J := A;
        Dec(A, J);
        AItem.Amount := J;
        AddItem(InvItems, AItem);
      end;
    end else AddItem(InvItems, AItem);
  end else AddItem(InvItems, AItem);
end;

function Items_Inventory_DeleteItem(Index: Integer; var AItem: Item): Integer; stdcall;
begin
  Result := IntFalse;
  if IndexInRange(InvItems, Index) then
  begin
    AItem := DelItem(InvItems, Index);
    Result := IntTrue;
  end;
end;

function Items_Inventory_EquipItem(Index: Integer): Integer; stdcall;
var
  I, FSlot: Integer;
begin
  Result := -1;
  if IndexInRange(InvItems, Index) then
  begin
    FSlot := InvItems[Index].SlotID;
    for I := 0 to Length(InvItems) - 1 do
    begin
      if (InvItems[I].SlotID = FSlot)
        and (InvItems[I].Equipment = IntTrue) then
      begin
        InvItems[I].Equipment := IntFalse;
        Result := I;
        Break;
      end;
    end;
    InvItems[Index].Equipment := IntTrue;
  end;
end;

function Items_Inventory_UnEquipItem(Index: Integer): Integer; stdcall;
begin
  Result := IntFalse;
  if IndexInRange(InvItems, Index) then
    if (InvItems[Index].Equipment = IntTrue) then
    begin
      InvItems[Index].Equipment := IntFalse;
      Result := IntTrue;
    end;
end;

procedure Items_Inventory_SetSlotCount(ACount: Integer); stdcall;
begin
  SlotMax := ACount;
  if (SlotMax < 1) then SlotMax := 1;
end;

function Items_Inventory_GetSlotCount: Integer; stdcall;
begin
  Result := SlotMax;
end;

end.
