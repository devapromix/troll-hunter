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

  procedure AddDoor(AXDoor, AYDoor: UInt);
  begin
    if AHasDoor then
      Exit;
    AMap.SetTileEnum(AXDoor, AYDoor, AZ, teDoor);
    AHasDoor := True;
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
      4: AddDoor(AX, AY - (H div 2));
      5: AddDoor(AX + (H div 2), AY);
      6: AddDoor(AX - (H div 2), AY);
      7: AddDoor(AX, AY + (H div 2));
    end;
    Exit;
  end;

  if F then
    if AX <= CX then
      AddDoor(AX + (W div 2), AY)
    else
      AddDoor(AX - (W div 2), AY)
  else if AY <= CY then
    AddDoor(AX, AY + (H div 2))
  else
    AddDoor(AX, AY - (H div 2));
end;

end.
