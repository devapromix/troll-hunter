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
  W, H, X, Y, PX, PY: UInt;
  LDoorHorizontal: boolean;

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

  procedure SetDoor(AIsHorizontal: boolean; AFixed: UInt);
  begin
    if AIsHorizontal then
      AMap.SetTileEnum(RandomRange(PX + 2, PX + W - 1), AFixed, AZ, teDoor)
    else
      AMap.SetTileEnum(AFixed, RandomRange(PY + 2, PY + H - 1), AZ, teDoor);
  end;

  procedure AddDoor(AIsHorizontal: boolean; AFixed: UInt);
  begin
    if AHasDoor then
      Exit;
    SetDoor(AIsHorizontal, AFixed);
    LDoorHorizontal := AIsHorizontal;
    AHasDoor := True;
  end;

  procedure AddPartition;
  var
    LX, LY, LStart, LSize, LPos: UInt;
  begin
    if Random(2) = 0 then
      Exit;
    if LDoorHorizontal then
    begin
      LStart := PY;
      LSize := H;
    end
    else
    begin
      LStart := PX;
      LSize := W;
    end;
    if LSize < 6 then
      Exit;
    LPos := RandomRange(LStart + 3, LStart + LSize - 2);
    for LX := PX + 1 to PX + W - 1 do
      for LY := PY + 1 to PY + H - 1 do
        if (LDoorHorizontal and (LY = LPos)) or (not LDoorHorizontal and (LX = LPos)) then
          AMap.SetTileEnum(LX, LY, AZ, teWoodenWall);
    SetDoor(LDoorHorizontal, LPos);
  end;

begin
  AHasDoor := False;
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
