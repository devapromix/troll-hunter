unit uMap;

interface

type
  TMapEnum = (deDarkWood, deGrayCave, deDeepCave, deBloodCave, deDrom);

const
  FinalDungeon = deDrom;

type
  TTile = record
    Symbol: Char;
    Name: string;
    Color: Cardinal;
  end;

type
  TTileEnum = (teDefaultFloor, teDefaultWall, teRock, teFloor1, teFloor2,
    teFloor3, teUpStairs, teDnStairs, teWater, teStoneWall, teWoodenWall,
    teStoneFloor, teWoodenFloor, teDoor, teGate);

const
  StopTiles = [teDefaultWall, teStoneWall, teWoodenWall];
  FreeTiles = [teDefaultFloor, teRock, teFloor1, teFloor2, teFloor3, teUpStairs,
    teDnStairs, teWater];
  VillageTiles = [teStoneWall, teWoodenWall, teStoneFloor, teWoodenFloor,
    teDoor, teGate];
  SpawnTiles = [teDefaultFloor, teRock, teFloor1, teFloor2, teFloor3, teWater];

var
  Tile: array [TTileEnum, TMapEnum] of TTile;

type
  TMap = class(TObject)
  private
    FCurrent: TMapEnum;
    FVis: array [TMapEnum] of Boolean;
    FMap: array [Byte, Byte, TMapEnum] of TTileEnum;
    FFog: array [Byte, Byte, TMapEnum] of Boolean;
    FFOV: array [Byte, Byte] of Boolean;
    procedure AddSpot(AX, AY: Byte; ASize: Word; AZ: TMapEnum;
      ABaseTileEnum, ATileEnum: TTileEnum);
    procedure AddTiles(AX, AY: Byte; AZ: TMapEnum; AType: Byte; ADen: Word;
      ABaseTileEnum, ATileEnum: TTileEnum);
    procedure AddTile(ASymbol: Char; AName: string; AColor: Cardinal;
      ATile: TTileEnum; AZ: TMapEnum);
    procedure InitTiles;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetVis(const AZ: TMapEnum; const Value: Boolean);
    function GetVis(const AZ: TMapEnum): Boolean;
    procedure Clear(Z: TMapEnum; ATileEnum: TTileEnum);
    procedure Gen;
    property Current: TMapEnum read FCurrent write FCurrent;
    function InMap(AX, AY: Integer): Boolean;
    function InView(AX, AY: Integer): Boolean;
    function GetFog(AX, AY: Byte): Boolean;
    procedure SetFog(AX, AY: Byte; AFlag: Boolean);
    procedure ClearFOV;
    function GetFOV(AX, AY: Byte): Boolean;
    procedure SetFOV(AX, AY: Byte; AFlag: Boolean);
    function GetTile(AX, AY: Byte): TTile; overload;
    function GetTile(ATileEnum: TTileEnum): TTile; overload;
    procedure SetTileEnum(AX, AY: Byte; AZ: TMapEnum; ATileEnum: TTileEnum);
    function GetTileEnum(AX, AY: Byte; AZ: TMapEnum): TTileEnum;
    function GetName: string;
  end;

var
  IsBoss: Boolean = False;
  IsRare: Boolean = False;

var
  Map: TMap = nil;

implementation

uses SysUtils, Math, Types, uPlayer, uMob, uItem, GNUGetText, uTerminal, uGame;

{ TMap }

procedure TMap.InitTiles;
begin
  // DefaultFloor
  AddTile('"', _('Grass'), $FF113311, teDefaultFloor, deDarkWood);
  AddTile(':', _('Dirt'), $FF331133, teDefaultFloor, deGrayCave);
  AddTile('.', _('Stone'), $FF222111, teDefaultFloor, deDeepCave);
  AddTile(';', _('Stone'), $FF330000, teDefaultFloor, deBloodCave);
  AddTile('~', _('Stone'), $FF002200, teDefaultFloor, deDrom);
  // DefaultWall
  AddTile('T', _('Tree'), $FF006622, teDefaultWall, deDarkWood);
  AddTile('#', _('Wall'), $FF444422, teDefaultWall, deGrayCave);
  AddTile('#', _('Wall'), $FF222133, teDefaultWall, deDeepCave);
  AddTile('#', _('Wall'), $FF322118, teDefaultWall, deBloodCave);
  AddTile('#', _('Wall'), $FF112211, teDefaultWall, deDrom);
  // Rock
  AddTile('^', _('Rock'), $FF556655, teRock, deDarkWood);
  AddTile(':', _('Wall'), $FF444422, teRock, deGrayCave);
  AddTile(':', _('Wall'), $FF222133, teRock, deDeepCave);
  AddTile(':', _('Wall'), $FF322118, teRock, deBloodCave);
  AddTile(':', _('Wall'), $FF112233, teRock, deDrom);
  // Floor #1
  AddTile('"', _('Grass'), $FF446644, teFloor1, deDarkWood);
  AddTile('"', _('Grass'), $FF99AA99, teFloor1, deGrayCave);
  AddTile('.', _('Stone'), $FF224422, teFloor1, deDeepCave);
  AddTile(';', _('Stone'), $FF225533, teFloor1, deBloodCave);
  AddTile('~', _('Stone'), $FF228833, teFloor1, deDrom);
  // Floor #2
  AddTile('"', _('Grass'), $FF447755, teFloor2, deDarkWood);
  AddTile('"', _('Grass'), $FF779977, teFloor2, deGrayCave);
  AddTile('.', _('Stone'), $FF22CC44, teFloor2, deDeepCave);
  AddTile(';', _('Stone'), $FF333322, teFloor2, deBloodCave);
  AddTile('~', _('Stone'), $FF334422, teFloor2, deDrom);
  // Floor #3
  AddTile('"', _('Grass'), $FF778866, teFloor3, deDarkWood);
  AddTile('"', _('Grass'), $FF668866, teFloor3, deGrayCave);
  AddTile('.', _('Stone'), $FF338855, teFloor3, deDeepCave);
  AddTile(';', _('Stone'), $FF223333, teFloor3, deBloodCave);
  AddTile('~', _('Stone'), $FF226622, teFloor3, deDrom);
  // Up Stairs
  AddTile('*', _('Stairs'), $FFFFFF00, teUpStairs, deDarkWood);
  AddTile('<', _('Stairs'), $FFEEEE00, teUpStairs, deGrayCave);
  AddTile('<', _('Stairs'), $FFDDDD00, teUpStairs, deDeepCave);
  AddTile('<', _('Stairs'), $FFCCCC00, teUpStairs, deBloodCave);
  AddTile('<', _('Stairs'), $FFBBBB00, teUpStairs, deDrom);
  // Down Stairs
  AddTile('*', _('Stairs'), $FFFFFF00, teDnStairs, deDarkWood);
  AddTile('>', _('Stairs'), $FFEEEE00, teDnStairs, deGrayCave);
  AddTile('>', _('Stairs'), $FFDDDD00, teDnStairs, deDeepCave);
  AddTile('>', _('Stairs'), $FFCCCC00, teDnStairs, deBloodCave);
  AddTile('>', _('Stairs'), $FFBBBB00, teDnStairs, deDrom);
  // Water
  AddTile('=', _('Water'), $FF333388, teWater, deDarkWood);
  AddTile('=', _('Water'), $FF333377, teWater, deGrayCave);
  AddTile('=', _('Water'), $FF222266, teWater, deDeepCave);
  AddTile('=', _('Water'), $FF222255, teWater, deBloodCave);
  AddTile('=', _('Water'), $FF222244, teWater, deDrom);
  // Stone Wall
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deDarkWood);
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deGrayCave);
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deDeepCave);
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deBloodCave);
  AddTile('#', _('Stone Wall'), $FF818F95, teStoneWall, deDrom);
  // Wooden Wall
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deDarkWood);
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deGrayCave);
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deDeepCave);
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deBloodCave);
  AddTile('#', _('Wooden Wall'), $FF776735, teWoodenWall, deDrom);
  // Stone Floor
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deDarkWood);
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deGrayCave);
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deDeepCave);
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deBloodCave);
  AddTile('.', _('Stone Floor'), $FF818F95, teStoneFloor, deDrom);
  // Wooden Floor
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deDarkWood);
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deGrayCave);
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deDeepCave);
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deBloodCave);
  AddTile('.', _('Wooden Floor'), $FF776735, teWoodenFloor, deDrom);
  // Door
  AddTile('+', _('Door'), $FF675725, teDoor, deDarkWood);
  AddTile('+', _('Door'), $FF675725, teDoor, deGrayCave);
  AddTile('+', _('Door'), $FF675725, teDoor, deDeepCave);
  AddTile('+', _('Door'), $FF675725, teDoor, deBloodCave);
  AddTile('+', _('Door'), $FF675725, teDoor, deDrom);
  // Gate
  AddTile('+', _('Gate'), $FF515F55, teGate, deDarkWood);
  AddTile('+', _('Gate'), $FF515F55, teGate, deGrayCave);
  AddTile('+', _('Gate'), $FF515F55, teGate, deDeepCave);
  AddTile('+', _('Gate'), $FF515F55, teGate, deBloodCave);
  AddTile('+', _('Gate'), $FF515F55, teGate, deDrom);
end;

procedure TMap.AddSpot(AX, AY: Byte; ASize: Word; AZ: TMapEnum;
  ABaseTileEnum, ATileEnum: TTileEnum);
var
  Z: TMapEnum;
  X, Y: Byte;
  I: Word;
begin
  X := AX;
  Y := AY;
  Z := AZ;
  ASize := EnsureRange(ASize, 49, 9999);
  for I := 0 to ASize do
  begin
    if (Round(Random(6)) = 1) and (X > 0) then
    begin
      X := X - 1;
      if (GetTileEnum(X, Y, Z) <> ABaseTileEnum) then
        Continue;
      SetTileEnum(X, Y, Z, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (X < High(Byte)) then
    begin
      X := X + 1;
      if (GetTileEnum(X, Y, Z) <> ABaseTileEnum) then
        Continue;
      SetTileEnum(X, Y, Z, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (Y > 0) then
    begin
      Y := Y - 1;
      if (GetTileEnum(X, Y, Z) <> ABaseTileEnum) then
        Continue;
      SetTileEnum(X, Y, Z, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (Y < High(Byte)) then
    begin
      Y := Y + 1;
      if (GetTileEnum(X, Y, Z) <> ABaseTileEnum) then
        Continue;
      SetTileEnum(X, Y, Z, ATileEnum);
    end;
  end;
end;

procedure TMap.AddTile(ASymbol: Char; AName: string; AColor: Cardinal;
  ATile: TTileEnum; AZ: TMapEnum);
begin
  with Tile[ATile, AZ] do
  begin
    Symbol := ASymbol;
    Name := AName;
    Color := AColor;
  end;
end;

procedure TMap.AddTiles(AX, AY: Byte; AZ: TMapEnum; AType: Byte; ADen: Word;
  ABaseTileEnum, ATileEnum: TTileEnum);
var
  K: Word;
  X, Y: Byte;
  Z: TMapEnum;

  procedure ModTile(const X, Y: Byte);
  begin
    if (GetTileEnum(X, Y, Z) = ABaseTileEnum) then
      SetTileEnum(X, Y, Z, ATileEnum);
  end;

begin
  X := AX;
  Y := AY;
  Z := AZ;
  AType := EnsureRange(AType, 2, 9);
  for K := 0 to ADen do
  begin
    if (Round(Random(AType)) = 1) and (X > 0) then
    begin
      X := X - 1;
      ModTile(X, Y);
    end;
    if (Round(Random(AType)) = 1) and (X < High(Byte)) then
    begin
      X := X + 1;
      ModTile(X, Y);
    end;
    if (Round(Random(AType)) = 1) and (Y > 0) then
    begin
      Y := Y - 1;
      ModTile(X, Y);
    end;
    if (Round(Random(AType)) = 1) and (Y < High(Byte)) then
    begin
      Y := Y + 1;
      ModTile(X, Y);
    end;
  end;
end;

procedure TMap.ClearFOV;
var
  X, Y: Integer;
begin
  for Y := Player.Y - Player.GetRadius to Player.Y + Player.GetRadius do
    for X := Player.X - Player.GetRadius to Player.X + Player.GetRadius do
      FFOV[EnsureRange(X, 0, High(Byte))][EnsureRange(Y, 0, High(Byte))
        ] := False;
end;

procedure TMap.Clear(Z: TMapEnum; ATileEnum: TTileEnum);
var
  X, Y: Byte;
begin
  for Y := 0 to High(Byte) do
    for X := 0 to High(Byte) do
    begin
      FMap[X][Y][Z] := ATileEnum;
      FFog[X][Y][Z] := True;
    end;
end;

constructor TMap.Create;
begin
  Self.Current := deDarkWood;
end;

destructor TMap.Destroy;
begin

  inherited;
end;

procedure TMap.Gen;
var
  I: Word;
  X, Y: Byte;
  Z: TMapEnum;

  procedure GenCave(D: Byte; C, V: Word);
  var
    I: Word;
  begin
    for I := 0 to C do
    begin
      repeat
        X := Math.RandomRange(10, High(Byte) - 10);
        Y := Math.RandomRange(10, High(Byte) - 10);
      until (GetTileEnum(X, Y, pred(Z)) = teDefaultFloor);
      Self.AddTiles(X, Y, Z, D, V, teDefaultWall, teDefaultFloor);
      SetTileEnum(X, Y, pred(Z), teDnStairs);
      SetTileEnum(X, Y, Z, teUpStairs);
    end;
  end;

  procedure AddArea(ADeep: TMapEnum; ABaseTileEnum, ATileEnum: TTileEnum);
  var
    X, Y: Byte;
  begin
    repeat
      X := Math.RandomRange(10, High(Byte) - 10);
      Y := Math.RandomRange(10, High(Byte) - 10);
    until (GetTileEnum(X, Y, ADeep) = ABaseTileEnum);
    AddSpot(X, Y, Math.RandomRange(49, High(Byte)), ADeep, ABaseTileEnum,
      ATileEnum);
  end;

  procedure AddFrame(AX, AY, AW, AH: Byte; ABaseTileEnum: TTileEnum);
  var
    X, Y: Byte;
    PX, PY: Byte;
  begin
    PX := AX - (AW div 2);
    PY := AY - (AH div 2);
    for X := PX to PX + AW do
      for Y := PY to PY + AH do
        if not (((X > PX) and (X < (PX + AW))) and ((Y > PY) and (Y < (PY + AH))))
        then SetTileEnum(X, Y, Z, ABaseTileEnum);
  end;

  procedure AddRect(AX, AY, AW, AH: Byte;
    AFloorTileEnum, AWallTileEnum: TTileEnum; IsFog: Boolean = False);
  var
    X, Y: Byte;
    PX, PY: Byte;
  begin
    PX := AX - (AW div 2);
    PY := AY - (AH div 2);
    for X := PX to PX + AW do
      for Y := PY to PY + AH do
      begin
        if IsFog then Self.SetFog(X, Y, False);
        if (((X > PX) and (X < (PX + AW))) and ((Y > PY) and (Y < (PY + AH))))
        then
          SetTileEnum(X, Y, Z, AFloorTileEnum)
        else
          SetTileEnum(X, Y, Z, AWallTileEnum);
      end;
  end;

  procedure AddNPC(AX, AY: Byte);
  begin
    Mobs.Add(Self.Current, AX, AY, fcNPC, Ord(npcNPC));
  end;

  procedure AddHouse(AX, AY, CX, CY, D: Byte; AV: Boolean; F: Boolean);
  var
    W, H: Byte;
    IsDoor: Boolean;

    procedure AddDoor(AX, AY: Byte);
    begin
      if IsDoor then Exit;
      SetTileEnum(AX, AY, Z, teDoor);
      IsDoor := True;
    end;

  begin
    IsDoor := False;
    W := IfThen(AV, 8, RandomRange(2, 5) * 2);
    H := IfThen(AV, 8, RandomRange(2, 5) * 2);
    AddRect(AX, AY, W, H, teWoodenFloor, teWoodenWall);
    // Add door
    if AV then
    begin
      case D of
        4:
          AddDoor(AX, AY - (H div 2));
        5:
          AddDoor(AX + (H div 2), AY);
        6:
          AddDoor(AX - (H div 2), AY);
        7:
          AddDoor(AX, AY + (H div 2));
      end;
      Exit;
    end;
    if F then
      if (AX <= CX) then
        AddDoor(AX + (W div 2), AY)
      else
        AddDoor(AX - (W div 2), AY)
    else
      if (AY <= CY) then
        AddDoor(AX, AY + (H div 2))
      else
      AddDoor(AX, AY - (H div 2));
  end;

  procedure AddVillage(AX, AY: Byte);
  var
    I, J, T, X, Y, PX, PY: Byte;
    HP: array [0 .. 7] of Boolean;
  const
    House: array [0 .. 7] of TPoint = ((X: - 10; Y: - 10;), (X: 10; Y: - 10;
      ), (X: - 10; Y: 10;), (X: 10; Y: 10;), (X: 0; Y: 10;), (X: - 10; Y: 0;
      ), (X: 10; Y: 0;), (X: 0; Y: - 10;));

    procedure AddGate(AX, AY: Byte; SX, SY: ShortInt);
    begin
      SetTileEnum(AX + SX, AY + SY, Z, teGate);
      if (SX = 0) then
      begin
        SetTileEnum(AX + 1, AY + SY, Z, teGate);
        SetTileEnum(AX - 1, AY + SY, Z, teGate);
      end;
      if (SY = 0) then
      begin
        SetTileEnum(AX + SX, AY + 1, Z, teGate);
        SetTileEnum(AX + SX, AY - 1, Z, teGate);
      end;
    end;

  begin
    // Save to log
    Game.Log(Format('Village: %dx%d', [AX, AY]));
    //
    AddFrame(AX, AY, 34, 34, teDefaultFloor);
    AddRect(AX, AY, 32, 32, teStoneFloor, teStoneWall, True);
    for I := 0 to High(House) do
      HP[I] := False;
    // Add gate
    J := Math.RandomRange(4, 8);
    case J of
      4:
        AddGate(AX, AY, 0, -16);
      5:
        AddGate(AX, AY, 16, 0);
      6:
        AddGate(AX, AY, -16, 0);
      7:
        AddGate(AX, AY, 0, 16);
    end;
    PX := AX - House[J].X;
    PY := AY - House[J].Y;
    AddRect(PX, PY, 10, 10, teStoneFloor,
      teStoneFloor);
    HP[J] := True;
    // Add houses
    T := 0;
    while (T < High(House)) do
    begin
      I := Math.RandomRange(0, 8);
      X := AX - House[I].X;
      Y := AY - House[I].Y;
      if not HP[I] then
      begin
        AddHouse(X, Y, AX, AY, J, I = (10 - J + 1),
          (J = 4) or (J = 7));
        AddNPC(X, Y);
        HP[I] := True;
        Inc(T);
      end;
    end;
  end;

begin
  InitTiles();
  for Z := Low(TMapEnum) to High(TMapEnum) do
  begin
    Self.SetVis(Z, False);
    case Z of
      deDarkWood:
        begin
          Self.SetVis(Z, True);
          Self.Clear(Z, teDefaultFloor);
          for I := 0 to 9999 do
            Self.SetTileEnum(Math.RandomRange(0, High(Byte)),
              Math.RandomRange(0, High(Byte)), Z, teDefaultWall);
          Player.X := RandomRange(25, High(Byte) - 25);
          Player.Y := RandomRange(25, High(Byte) - 25);
          AddVillage(Player.X, Player.Y);
        end;
      deGrayCave:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(9, 49, 4999);
        end;
      deDeepCave:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(6, 39, 3999);
          AddVillage(Player.X, Player.Y);
        end;
      deBloodCave:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(3, 29, 2999);
        end;
      deDrom:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(2, 19, 1999);
        end;
    end;
    for I := 0 to 9 do
      AddArea(Z, teDefaultFloor, teWater);
    for I := 0 to 19 do
      AddArea(Z, teDefaultFloor, teRock);
    for I := 0 to 29 do
      AddArea(Z, teDefaultFloor, teFloor1);
    for I := 0 to 39 do
      AddArea(Z, teDefaultFloor, teFloor2);
    for I := 0 to 49 do
      AddArea(Z, teDefaultFloor, teFloor3);
  end;

  for Z := Low(TMapEnum) to High(TMapEnum) do
  begin
    // Add mobs
    IsBoss := False;
    for I := 0 to 255 do
      Mobs.Add(Z);
    // Add items
    IsRare := False;
    for I := 0 to 255 do
      Items.Add(Z);
  end;
end;

function TMap.GetTile(ATileEnum: TTileEnum): TTile;
begin
  Result := Tile[ATileEnum][Current];
end;

function TMap.GetTile(AX, AY: Byte): TTile;
begin
  Result := Tile[FMap[AX][AY][Current]][Current];
end;

function TMap.GetName: string;
begin
  if (GetTileEnum(Player.X, Player.Y, Current) in VillageTiles) then
  begin
    case Current of
      deDarkWood:
        Result := _('Village Dork');
      deDeepCave:
        Result := _('Village Elatrom');
    end;
    Exit;
  end;
  case Current of
    deDarkWood:
      Result := _('Dark Wood');
    deGrayCave:
      Result := _('Gray Cave');
    deDeepCave:
      Result := _('Deep Cave');
    deBloodCave:
      Result := _('Blood Cave');
    deDrom:
      Result := _('Drom');
  end;
end;

function TMap.GetTileEnum(AX, AY: Byte; AZ: TMapEnum): TTileEnum;
begin
  Result := FMap[AX][AY][AZ];
end;

function TMap.GetVis(const AZ: TMapEnum): Boolean;
begin
  Result := FVis[AZ];
end;

procedure TMap.SetTileEnum(AX, AY: Byte; AZ: TMapEnum; ATileEnum: TTileEnum);
begin
  FMap[AX][AY][AZ] := ATileEnum;
end;

function TMap.GetFog(AX, AY: Byte): Boolean;
begin
  Result := FFog[AX][AY][Current];
end;

procedure TMap.SetFog(AX, AY: Byte; AFlag: Boolean);
begin
  FFog[AX][AY][Current] := AFlag;
end;

function TMap.InMap(AX, AY: Integer): Boolean;
begin
  Result := (AX >= 0) and (AY >= 0) and (AX <= High(Byte)) and
    (AY <= High(Byte))
end;

function TMap.InView(AX, AY: Integer): Boolean;
var
  PX, PY: Integer;
begin
  PX := View.Width div 2;
  PY := View.Height div 2;
  Result := (AX >= Player.X - PX) and (AY >= Player.Y - PY) and
    (AX <= Player.X + PX - 1) and (AY <= Player.Y + PY - 1);
end;

function TMap.GetFOV(AX, AY: Byte): Boolean;
begin
  Result := FFOV[AX][AY];
end;

procedure TMap.SetFOV(AX, AY: Byte; AFlag: Boolean);
begin
  FFOV[AX][AY] := AFlag;
end;

procedure TMap.SetVis(const AZ: TMapEnum; const Value: Boolean);
begin
  FVis[AZ] := Value;
end;

initialization

Map := TMap.Create;

finalization

FreeAndNil(Map);

end.
