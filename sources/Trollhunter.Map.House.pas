unit Trollhunter.Map.House;

interface

uses
  Trollhunter.Types,
  Trollhunter.Map;

procedure GenerateHouse(AMap: TMap; AX, AY, CX, CY: UInt; AZ: TMapEnum; D: UInt;
  AV, F: boolean; out AHasDoor: boolean);

implementation

uses
  Math;

procedure GenerateHouse(AMap: TMap; AX, AY, CX, CY: UInt; AZ: TMapEnum; D: UInt;
  AV, F: boolean; out AHasDoor: boolean);
var
  W, H, PX, PY, LDoorFixed, LDoorAcross: UInt;
  LDoorHorizontal: boolean;
  LGround: TTileEnum;

  procedure AddRect(AWidth, AHeight: UInt);
  var
    RX, RY: UInt;
  begin
    PX := AX - (AWidth div 2);
    PY := AY - (AHeight div 2);
    for RX := PX to PX + AWidth do
      for RY := PY to PY + AHeight do
        if (((RX > PX) and (RX < (PX + AWidth))) and
            ((RY > PY) and (RY < (PY + AHeight)))) then
          AMap.SetTileEnum(RX, RY, AZ, teWoodenFloor)
        else
          AMap.SetTileEnum(RX, RY, AZ, teWoodenWall);
  end;

  procedure SetTileAt(AAlong, AAcross: UInt; ATileEnum: TTileEnum);
  begin
    if LDoorHorizontal then
      AMap.SetTileEnum(AAcross, AAlong, AZ, ATileEnum)
    else
      AMap.SetTileEnum(AAlong, AAcross, AZ, ATileEnum);
  end;

  procedure PlaceDoor(AAlong, AFrom, ATo: UInt);
  begin
    LDoorAcross := RandomRange(AFrom, ATo);
    SetTileAt(AAlong, LDoorAcross, teDoor);
  end;

  procedure SetDoor(AAlong: UInt);
  begin
    if LDoorHorizontal then
      PlaceDoor(AAlong, PX + 2, PX + W - 1)
    else
      PlaceDoor(AAlong, PY + 2, PY + H - 1);
  end;

  procedure AddDoor(AIsHorizontal: boolean; AFixed: UInt);
  begin
    if AHasDoor then
      Exit;
    LDoorHorizontal := AIsHorizontal;
    LDoorFixed := AFixed;
    SetDoor(AFixed);
    AHasDoor := True;
  end;

  procedure AddWing(AStart, ASize, AAStart, AASize, APos: UInt);
  var
    LNarrow, LWall, LCutFrom, LCutTo, LDoorFrom, LDoorTo, LAlongFrom,
      LAlongTo, LAlong, LAcross: UInt;
    LNarrowAtStart, LNearNarrow: boolean;
  begin
    LNarrow := AASize - 2 * RandomRange(1, AASize div 2 - 1);
    if Random(2) = 0 then
    begin
      LWall := AAStart + LNarrow;
      LCutFrom := LWall + 1;
      LCutTo := AAStart + AASize;
      LDoorFrom := AAStart + 2;
    end
    else
    begin
      LWall := AAStart + AASize - LNarrow;
      LCutFrom := AAStart;
      LCutTo := LWall - 1;
      LDoorFrom := LWall + 2;
    end;
    LDoorTo := LDoorFrom + LNarrow - 3;
    LNarrowAtStart := Random(2) = 0;
    if LNarrowAtStart then
    begin
      LAlongFrom := AStart;
      LAlongTo := APos - 1;
    end
    else
    begin
      LAlongFrom := APos + 1;
      LAlongTo := AStart + ASize;
    end;
    LNearNarrow := LNarrowAtStart = (LDoorFixed = AStart);
    if LNearNarrow then
      SetTileAt(LDoorFixed, LDoorAcross, teWoodenWall);
    for LAlong := LAlongFrom to LAlongTo do
    begin
      SetTileAt(LAlong, LWall, teWoodenWall);
      for LAcross := LCutFrom to LCutTo do
        SetTileAt(LAlong, LAcross, LGround);
    end;
    if LNearNarrow then
      PlaceDoor(LDoorFixed, LDoorFrom, LDoorTo);
    PlaceDoor(APos, LDoorFrom, LDoorTo);
  end;

  procedure AddPartition;
  var
    LX, LY, LStart, LSize, LPos, LAStart, LASize: UInt;
  begin
    if Random(2) = 0 then
      Exit;
    if LDoorHorizontal then
    begin
      LStart := PY;
      LSize := H;
      LAStart := PX;
      LASize := W;
    end
    else
    begin
      LStart := PX;
      LSize := W;
      LAStart := PY;
      LASize := H;
    end;
    if LSize < 6 then
      Exit;
    LPos := RandomRange(LStart + 3, LStart + LSize - 2);
    for LX := PX + 1 to PX + W - 1 do
      for LY := PY + 1 to PY + H - 1 do
        if (LDoorHorizontal and (LY = LPos)) or (not LDoorHorizontal and (LX = LPos)) then
          AMap.SetTileEnum(LX, LY, AZ, teWoodenWall);
    if (LASize >= 6) and (Random(2) = 0) then
      AddWing(LStart, LSize, LAStart, LASize, LPos)
    else
      SetDoor(LPos);
  end;

begin
  AHasDoor := False;
  LGround := AMap.GetTileEnum(AX, AY, AZ);
  W := IfThen(AV, 8, RandomRange(2, 5) * 2);
  H := IfThen(AV, 8, RandomRange(2, 5) * 2);
  AddRect(W, H);

  // Add door
  if AV then
  begin
    case D of
      4: AddDoor(True, AY - (H div 2));
      5: AddDoor(False, AX + (H div 2));
      6: AddDoor(False, AX - (H div 2));
      7: AddDoor(True, AY + (H div 2));
    end;
  end
  else if F then
  begin
    if AX <= CX then
      AddDoor(False, AX + (W div 2))
    else
      AddDoor(False, AX - (W div 2));
  end
  else if AY <= CY then
    AddDoor(True, AY + (H div 2))
  else
    AddDoor(True, AY - (H div 2));
  AddPartition;
end;

end.
