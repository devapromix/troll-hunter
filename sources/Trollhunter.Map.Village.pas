unit Trollhunter.Map.Village;

interface

uses
  Trollhunter.Types,
  Trollhunter.Map;

procedure GenerateVillage(AMap: TMap; AX, AY: UInt; AZ: TMapEnum);
procedure GenerateCellar(AMap: TMap; AZ: TMapEnum);

implementation

uses
  SysUtils,
  Math,
  Types,
  Trollhunter.Player,
  Trollhunter.Mob,
  Trollhunter.Game,
  Trollhunter.Map.House;

var
  BNPC: array [0 .. 6] of boolean;
  CellarPos: TPoint;

procedure AddFrame(AMap: TMap; AX, AY, AW, AH: UInt; AZ: TMapEnum;
  ABaseTileEnum: TTileEnum);
var
  LX, LY: UInt;
  LPX, LPY: UInt;
begin
  LPX := AX - (AW div 2);
  LPY := AY - (AH div 2);
  for LX := LPX to LPX + AW do
    for LY := LPY to LPY + AH do
      if not (((LX > LPX) and (LX < (LPX + AW))) and ((LY > LPY) and (LY < (LPY + AH))))
      then
        AMap.SetTileEnum(LX, LY, AZ, ABaseTileEnum);
end;

procedure AddRect(AMap: TMap; AX, AY, AW, AH: UInt; AZ: TMapEnum;
  AFloorTileEnum, AWallTileEnum: TTileEnum; AIsFog: boolean = False);
var
  LX, LY: UInt;
  LPX, LPY: UInt;
begin
  LPX := AX - (AW div 2);
  LPY := AY - (AH div 2);
  for LX := LPX to LPX + AW do
    for LY := LPY to LPY + AH do
    begin
      if AIsFog then
        AMap.SetFog(LX, LY, False);
      if (((LX > LPX) and (LX < (LPX + AW))) and ((LY > LPY) and (LY < (LPY + AH))))
      then
        AMap.SetTileEnum(LX, LY, AZ, AFloorTileEnum)
      else
        AMap.SetTileEnum(LX, LY, AZ, AWallTileEnum);
    end;
end;

procedure AddNPC(AMap: TMap; AX, AY: UInt; AZ: TMapEnum);
var
  LIndex, LX, LY, LArcaneX, LArcaneY: Int;
begin
  repeat
    LIndex := Math.RandomRange(0, 7);
  until not BNPC[LIndex];

  // Place the main NPC on a random walkable tile inside the house.
  repeat
    LX := Int(AX) + Math.RandomRange(-4, 5);
    LY := Int(AY) + Math.RandomRange(-4, 5);
  until AMap.GetTileEnum(LX, LY, AZ) = teWoodenFloor;

  Mobs.Add(AMap.Current, LX, LY, fcNPC,
    Ord(mbEldan_2the_magic_trader3) + LIndex);
  BNPC[LIndex] := True;

  if (LIndex = 0) then
  begin
    repeat
      LArcaneX := Int(AX) + Math.RandomRange(-4, 5);
      LArcaneY := Int(AY) + Math.RandomRange(-4, 5);
    until (AMap.GetTileEnum(LArcaneX, LArcaneY, AZ) = teWoodenFloor) and
      ((LArcaneX <> LX) or (LArcaneY <> LY));
    Mobs.Add(AMap.Current, LArcaneX, LArcaneY, fcNPC,
      Ord(mbArno_2the_arcane_trader3));
  end;
end;

procedure AddCellarNPC(AMap: TMap; AZ: TMapEnum; AMob: TMobEnum);
var
  LX, LY: UInt;
begin
  repeat
    LX := CellarPos.X + Math.RandomRange(0, 3) - 1;
    LY := CellarPos.Y + Math.RandomRange(0, 3) - 1;
  until (AMap.GetTileEnum(LX, LY, AZ) = teWoodenFloor) and
    ((LX <> CellarPos.X) or (LY <> CellarPos.Y));
  Mobs.Add(AZ, LX, LY, fcNPC, Ord(AMob));
end;

procedure GenerateVillageLayout(AMap: TMap; AX, AY: UInt; AZ: TMapEnum;
  AHasCutCorner: boolean);
var
  I, J, T, X, Y, LPX, LPY: UInt;
  LHasHouseDoor: boolean;
  LEmptyPlot: UInt;
  LHP: array [0 .. 7] of boolean;
  LHouseCount: UInt;
  LHousePos: array [0 .. 7] of TPoint;
const
  CVillageRadius = 22;
  CutEdge = 8;
  GateLaneOffset = 7;
  CHouse: array [0 .. 7] of TPoint = (
    (X: -15; Y: -15; ),
    (X: 15; Y: -15; ),
    (X: -15; Y: 15; ),
    (X: 15; Y: 15; ),
    (X: 0; Y: 15; ),
    (X: -15; Y: 0; ),
    (X: 15; Y: 0; ),
    (X: 0; Y: -15; )
    );

  procedure AddStash(AX, AY: UInt);
  begin
    AMap.SetTileEnum(AX, AY, AZ, teStash);
  end;

  procedure AddGate(AX, AY: UInt; SX, SY: shortint);
  begin
    AMap.SetTileEnum(AX + SX, AY + SY, AZ, teGate);
    if (SX = 0) then
    begin
      AMap.SetTileEnum(AX + 1, AY + SY, AZ, teGate);
      AMap.SetTileEnum(AX - 1, AY + SY, AZ, teGate);
    end;
    if (SY = 0) then
    begin
      AMap.SetTileEnum(AX + SX, AY + 1, AZ, teGate);
      AMap.SetTileEnum(AX + SX, AY - 1, AZ, teGate);
    end;
  end;

  procedure BuildCutCorner(ACorner: UInt; AHorizontalGate: boolean);
  var
    DX, DY, SX, SY, TX, TY: Int;
    GateX, GateY: UInt;
    Tile: TTileEnum;
  begin
    SX := -Sign(CHouse[ACorner].X);
    SY := -Sign(CHouse[ACorner].Y);
    for DX := -CVillageRadius to CVillageRadius do
      for DY := -CVillageRadius to CVillageRadius do
      begin
        TX := DX * SX;
        TY := DY * SY;
        if (TX > CutEdge) and (TY > CutEdge) then
          Continue;
        if (Abs(DX) = CVillageRadius) or (Abs(DY) = CVillageRadius) or
          ((TX = CutEdge) and (TY >= CutEdge)) or
          ((TY = CutEdge) and (TX >= CutEdge)) then
          Tile := teStoneWall
        else
          Tile := teStoneFloor;
        AMap.SetTileEnum(Int(AX) + DX, Int(AY) + DY, AZ, Tile);
        AMap.SetFog(Int(AX) + DX, Int(AY) + DY, False);
      end;

    if AHorizontalGate then
    begin
      if SY < 0 then
        J := 4
      else
        J := 7;
      GateX := Int(AX) - GateLaneOffset * SX;
      AddGate(GateX, AY, 0, CVillageRadius * SY);
      GateY := Int(AY) + (CVillageRadius + 1) * SY;
      for DX := -1 to 1 do
        AMap.SetTileEnum(Int(GateX) + DX, GateY, AZ, teDefaultFloor);
    end
    else
    begin
      if SX < 0 then
        J := 6
      else
        J := 5;
      GateY := Int(AY) - GateLaneOffset * SY;
      AddGate(AX, GateY, CVillageRadius * SX, 0);
      GateX := Int(AX) + (CVillageRadius + 1) * SX;
      for DY := -1 to 1 do
        AMap.SetTileEnum(GateX, Int(GateY) + DY, AZ, teDefaultFloor);
    end;
    AddStash(AX, AY);
  end;

begin
  for I := 0 to 6 do
    BNPC[I] := False;
  Game.Log(Format('Village: %dx%d', [AX, AY]));
  Player.X := AX;
  Player.Y := AY;

  if AHasCutCorner then
  begin
    LEmptyPlot := RandomRange(0, 4);
    BuildCutCorner(LEmptyPlot, (LEmptyPlot mod 2) = 0);
  end
  else
  begin
    AddFrame(AMap, AX, AY, 2 * (CVillageRadius + 1), 2 * (CVillageRadius + 1),
      AZ, teDefaultFloor);
    AddRect(AMap, AX, AY, 2 * CVillageRadius, 2 * CVillageRadius, AZ,
      teStoneFloor, teStoneWall, True);
    // Add gate and stash
    J := Math.RandomRange(4, 8);
    case J of
      4:
      begin
        AddGate(AX, AY, 0, -CVillageRadius);
        AddStash(AX + (RandomRange(1, 4) - 2), AY + 3);
      end;
      5:
      begin
        AddGate(AX, AY, CVillageRadius, 0);
        AddStash(AX - 3, AY + (RandomRange(1, 4) - 2));
      end;
      6:
      begin
        AddGate(AX, AY, -CVillageRadius, 0);
        AddStash(AX + 3, AY + (RandomRange(1, 4) - 2));
      end;
      7:
      begin
        AddGate(AX, AY, 0, CVillageRadius);
        AddStash(AX + (RandomRange(1, 4) - 2), AY - 3);
      end;
    end;
    LPX := AX - CHouse[J].X;
    LPY := AY - CHouse[J].Y;
    AddRect(AMap, LPX, LPY, 10, 10, AZ, teStoneFloor, teStoneFloor);
    LEmptyPlot := J;
  end;

  for I := 0 to High(CHouse) do
    LHP[I] := I = LEmptyPlot;
  // Add houses
  T := 0;
  LHouseCount := 0;
  while (T < High(CHouse)) do
  begin
    I := Math.RandomRange(0, 8);
    X := AX - CHouse[I].X;
    Y := AY - CHouse[I].Y;
    if not LHP[I] then
    begin
      GenerateHouse(AMap, X, Y, AX, AY, AZ, J, I = (10 - J + 1),
        (J = 4) or (J = 7), LHasHouseDoor);
      if LHasHouseDoor then
        AddNPC(AMap, X, Y, AZ);
      LHousePos[LHouseCount].X := X;
      LHousePos[LHouseCount].Y := Y;
      Inc(LHouseCount);
      LHP[I] := True;
      Inc(T);
    end;
  end;
  // Choose random house for cellar stairs
  if LHouseCount > 0 then
  begin
    I := Math.RandomRange(0, LHouseCount);
    CellarPos := LHousePos[I];
    repeat
      X := CellarPos.X + Math.RandomRange(0, 3) - 1;
      Y := CellarPos.Y + Math.RandomRange(0, 3) - 1;
    until (AMap.GetTileEnum(X, Y, AZ) = teWoodenFloor) and
      ((X <> CellarPos.X) or (Y <> CellarPos.Y));
    CellarPos := Point(X, Y);
    AMap.SetTileEnum(CellarPos.X, CellarPos.Y, AZ, teDnStoneStairs);
  end
  else
    CellarPos := Point(AX, AY);
end;

procedure GenVillageTypeA(AMap: TMap; AX, AY: UInt; AZ: TMapEnum);
begin
  GenerateVillageLayout(AMap, AX, AY, AZ, False);
end;

procedure GenVillageTypeB(AMap: TMap; AX, AY: UInt; AZ: TMapEnum);
begin
  GenerateVillageLayout(AMap, AX, AY, AZ, True);
end;

procedure GenerateVillage(AMap: TMap; AX, AY: UInt; AZ: TMapEnum);
begin
  if Random(4) = 0 then
    GenVillageTypeA(AMap, AX, AY, AZ)
  else
    GenVillageTypeB(AMap, AX, AY, AZ);
end;

procedure GenerateCellar(AMap: TMap; AZ: TMapEnum);
var
  LW, LH: UInt;
begin
  LW := RandomRange(5, 7);
  LH := RandomRange(5, 7);
  AddRect(AMap, CellarPos.X, CellarPos.Y, LW, LH, AZ, teWoodenFloor,
    teMossyWall);
  AMap.SetTileEnum(CellarPos.X, CellarPos.Y, AZ, teUpStoneStairs);

  AddCellarNPC(AMap, AZ, mbGorn_2the_thief3);
  AddCellarNPC(AMap, AZ, mbFenn_2the_pawnbroker3);
end;

end.
