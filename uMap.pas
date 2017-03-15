unit uMap;

interface

uses uCommon;

type
  TDeepEnum = (deDarkWood, deGrayCave, deDeepCave, deBloodCave, deDungeonOfDoom);

const
  DeepName: array [TDeepEnum] of string = (
    'Dark Wood', 'Gray Cave', 'Deep Cave', 'Blood Cave', 'Dungeon of Doom');

type
  TTile = record
    Symbol: Char;
    Name: string;
    Color: Cardinal;
  end;

type
  TTileEnum = (teDefaultFloor, teDefaultWall, teRock,
    teFloor1, teFloor2, teFloor3,
    teUpStairs, teDnStairs, teWater);

const
  StopTiles  = [teDefaultWall];
  FreeTiles  = [teDefaultFloor, teRock,
    teFloor1, teFloor2, teFloor3,
    teUpStairs, teDnStairs, teWater];
  SpawnTiles = [teDefaultFloor, teRock,
    teFloor1, teFloor2, teFloor3,
    teWater];

const
  Tile: array[TTileEnum, TDeepEnum] of TTile = (
  ( // DefaultFloor
    (Symbol: '"'; Name: 'Grass'; Color: $FF113311;), // Dark Wood
    (Symbol: ':'; Name: 'Dirt';  Color: $FF331133;), // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222111;), // Deep Cave
    (Symbol: ';'; Name: 'Stone'; Color: $FF330000;), // Blood Cave
    (Symbol: '~'; Name: 'Stone'; Color: $FF002200;)  // Dungeon of Doom
  ),
  ( // DefaultWall
    (Symbol: 'T'; Name: 'Tree';  Color: $FF006622;), // Dark Wood
    (Symbol: '#'; Name: 'Wall';  Color: $FF444422;), // Gray Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF222133;), // Deep Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF322118;), // Blood Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF112211;)  // Dungeon of Doom
  ),
  ( // Rock
    (Symbol: '^'; Name: 'Rock';  Color: $FF556655;), // Dark Wood
    (Symbol: ':'; Name: 'Wall';  Color: $FF444422;), // Gray Cave
    (Symbol: ':'; Name: 'Wall';  Color: $FF222133;), // Deep Cave
    (Symbol: ':'; Name: 'Wall';  Color: $FF322118;), // Blood Cave
    (Symbol: ';'; Name: 'Wall';  Color: $FF322118;)  // Dungeon of Doom
  ),
  ( // Floor1
    (Symbol: '"'; Name: 'Grass'; Color: $FF446644;), // Dark Wood
    (Symbol: '"'; Name: 'Grass'; Color: $FF99AA99;), // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF224422;), // Deep Cave
    (Symbol: ';'; Name: 'Stone'; Color: $FF225533;), // Blood Cave
    (Symbol: '~'; Name: 'Stone'; Color: $FF228833;)  // Dungeon of Doom
  ),
  ( // Floor2
    (Symbol: '"'; Name: 'Grass'; Color: $FF447755;), // Dark Wood
    (Symbol: '"'; Name: 'Grass'; Color: $FF779977;), // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF22CC44;), // Deep Cave
    (Symbol: ';'; Name: 'Stone'; Color: $FF333322;), // Blood Cave
    (Symbol: '~'; Name: 'Stone'; Color: $FF334422;)  // Dungeon of Doom
  ),
  ( // Floor3
    (Symbol: '"'; Name: 'Grass'; Color: $FF778866;), // Dark Wood
    (Symbol: '"'; Name: 'Grass'; Color: $FF668866;), // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF338855;), // Deep Cave
    (Symbol: ';'; Name: 'Stone'; Color: $FF223333;), // Blood Cave
    (Symbol: '~'; Name: 'Stone'; Color: $FF226622;)  // Dungeon of Doom
  ),
  ( // UpStairs
    (Symbol: '*'; Name: 'Stairs'; Color: $FFFFFF00;), // Dark Wood
    (Symbol: '<'; Name: 'Stairs'; Color: $FFEEEE00;), // Gray Cave
    (Symbol: '<'; Name: 'Stairs'; Color: $FFDDDD00;), // Deep Cave
    (Symbol: '<'; Name: 'Stairs'; Color: $FFCCCC00;), // Blood Cave
    (Symbol: '<'; Name: 'Stairs'; Color: $FFBBBB00;)  // Dungeon of Doom
  ),
  ( // DnStairs
    (Symbol: '*'; Name: 'Stairs'; Color: $FFFFFF00;), // Dark Wood
    (Symbol: '>'; Name: 'Stairs'; Color: $FFEEEE00;), // Gray Cave
    (Symbol: '>'; Name: 'Stairs'; Color: $FFDDDD00;), // Deep Cave
    (Symbol: '>'; Name: 'Stairs'; Color: $FFCCCC00;), // Blood Cave
    (Symbol: '>'; Name: 'Stairs'; Color: $FFBBBB00;)  // Dungeon of Doom
  ),
  ( // Water
    (Symbol: '='; Name: 'Water'; Color: $FF333388;), // Dark Wood
    (Symbol: '='; Name: 'Water'; Color: $FF333377;), // Gray Cave
    (Symbol: '='; Name: 'Water'; Color: $FF222266;), // Deep Cave
    (Symbol: '='; Name: 'Water'; Color: $FF222255;), // Blood Cave
    (Symbol: '='; Name: 'Water'; Color: $FF222244;)  // Dungeon of Doom
  )
  );

type
  TMap = class(TObject)
  private
    FDeep: TDeepEnum;
    FMap: array [Byte, Byte, TDeepEnum] of TTileEnum;
    FFog: array [Byte, Byte, TDeepEnum] of Boolean;
    FFOV: array [Byte, Byte] of Boolean;
    procedure AddSpot(AX, AY: Byte; ASize: Word; ADeep: TDeepEnum; ABaseTileEnum, ATileEnum: TTileEnum);
    procedure AddTiles(AX, AY: Byte; ADeep: TDeepEnum; AType: Byte; ADen: Word; ABaseTileEnum, ATileEnum: TTileEnum);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear(ADeep: TDeepEnum; ATileEnum: TTileEnum);
    procedure Gen;
    property Deep: TDeepEnum read FDeep write FDeep;
    function InMap(AX, AY: Integer): Boolean;
    function InView(AX, AY: Integer): Boolean;
    function GetFog(AX, AY: Byte): Boolean;
    procedure SetFog(AX, AY: Byte; AFlag: Boolean);
    procedure ClearFOV;
    function GetFOV(AX, AY: Byte): Boolean;
    procedure SetFOV(AX, AY: Byte; AFlag: Boolean);
    function GetTile(AX, AY: Byte): TTile; overload;
    function GetTile(ATileEnum: TTileEnum): TTile; overload;
    procedure SetTileEnum(AX, AY: Byte; ADeep: TDeepEnum; ATileEnum: TTileEnum);
    function GetTileEnum(AX, AY: Byte; ADeep: TDeepEnum): TTileEnum;
    function GetName: string;
  end;

var
  Map: TMap = nil;

implementation

uses Math, uPlayer, uMob, uItem;

{ TMap }

procedure TMap.AddSpot(AX, AY: Byte; ASize: Word; ADeep: TDeepEnum; ABaseTileEnum,
  ATileEnum: TTileEnum);
var
  X, Y: Byte;
  I: Word;
begin
  X := AX;
  Y := AY;
  ASize := Clamp(ASize, 49, 9999);
  for I := 0 to ASize do
  begin
    if (Round(Random(6)) = 1) and (X > 0) then
    begin
      X := X - 1;
      if (GetTileEnum(X, Y, ADeep) <> ABaseTileEnum) then Continue;
      SetTileEnum(X, Y, ADeep, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (X < High(Byte)) then
    begin
      X := X + 1;
      if (GetTileEnum(X, Y, ADeep) <> ABaseTileEnum) then Continue;
      SetTileEnum(X, Y, ADeep, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (Y > 0) then
    begin
      Y := Y - 1;
      if (GetTileEnum(X, Y, ADeep) <> ABaseTileEnum) then Continue;
      SetTileEnum(X, Y, ADeep, ATileEnum);
    end;
    if (Round(Random(6)) = 1) and (Y < High(Byte)) then
    begin
      Y := Y + 1;
      if (GetTileEnum(X, Y, ADeep) <> ABaseTileEnum) then Continue;
      SetTileEnum(X, Y, ADeep, ATileEnum);
    end;
  end;
end;

procedure TMap.AddTiles(AX, AY: Byte; ADeep: TDeepEnum; AType: Byte; ADen: Word; ABaseTileEnum, ATileEnum: TTileEnum);
var
  K: Word;
  X, Y: Byte;

  procedure ModTile(const X, Y: Byte);
  begin
    if (GetTileEnum(X, Y, ADeep) = ABaseTileEnum) then
      SetTileEnum(X, Y, ADeep, ATileEnum);
  end;

begin
  X := AX;
  Y := AY;
  AType := Clamp(AType, 2, 9);
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
      FFOV[Clamp(X, 0, High(Byte))][Clamp(Y, 0, High(Byte))] := False;
end;

procedure TMap.Clear(ADeep: TDeepEnum; ATileEnum: TTileEnum);
var
  X, Y: Byte;
begin
  for Y := 0 to High(Byte) do
    for X := 0 to High(Byte) do
    begin
      FMap[X][Y][ADeep] := ATileEnum;
      FFog[X][Y][ADeep] := True;
    end;
end;

constructor TMap.Create;
begin

end;

destructor TMap.Destroy;
begin

  inherited;
end;

procedure TMap.Gen;
var
  I: Word;
  X, Y: Byte;
  FDeep: TDeepEnum;

  procedure GenCave(D: Byte; C, V: Word);
  var
    I: Word;
  begin
    for I := 0 to C do
    begin
      repeat
        X := Math.RandomRange(10, High(Byte) - 10);
        Y := Math.RandomRange(10, High(Byte) - 10);
      until(GetTileEnum(X, Y, pred(FDeep)) = teDefaultFloor);
      Self.AddTiles(X, Y, FDeep, D, V, teDefaultWall, teDefaultFloor);
      SetTileEnum(X, Y, pred(FDeep), teDnStairs);
      SetTileEnum(X, Y, FDeep, teUpStairs);
    end;
  end;

  procedure AddArea(ADeep: TDeepEnum; ABaseTileEnum, ATileEnum: TTileEnum);
  var
    X, Y: Byte;
  begin
    repeat
      X := Math.RandomRange(10, High(Byte) - 10);
      Y := Math.RandomRange(10, High(Byte) - 10);
    until(GetTileEnum(X, Y, ADeep) = ABaseTileEnum);
    AddSpot(X, Y, Math.RandomRange(49, High(Byte)), ADeep, ABaseTileEnum, ATileEnum);
  end;

begin
  for FDeep := Low(TDeepEnum) to High(TDeepEnum) do
  begin
    case FDeep of
      deDarkWood:
      begin
        Self.Clear(FDeep, teDefaultFloor);
        for I := 0 to 9999 do
          Self.SetTileEnum(Math.RandomRange(0, High(Byte)),
            Math.RandomRange(0, High(Byte)), FDeep, teDefaultWall);
      end;
      deGrayCave:
      begin
        Self.Clear(FDeep, teDefaultWall);
        GenCave(9, 49, 4999);
      end;
      deDeepCave:
      begin
        Self.Clear(FDeep, teDefaultWall);
        GenCave(6, 39, 3999);
      end;
      deBloodCave:
      begin
        Self.Clear(FDeep, teDefaultWall);
        GenCave(3, 29, 2999);
      end;
      deDungeonOfDoom:
      begin
        Self.Clear(FDeep, teDefaultWall);
        GenCave(2, 19, 1999);
      end;
    end;
    for I := 0 to 9 do
      AddArea(FDeep, teDefaultFloor, teWater);
    for I := 0 to 19 do
      AddArea(FDeep, teDefaultFloor, teRock);
    for I := 0 to 29 do
      AddArea(FDeep, teDefaultFloor, teFloor1);
    for I := 0 to 39 do
      AddArea(FDeep, teDefaultFloor, teFloor2);
    for I := 0 to 49 do
      AddArea(FDeep, teDefaultFloor, teFloor3);
  end;
  repeat
    Player.X := RandomRange(64, High(Byte) - 64);
    Player.Y := RandomRange(64, High(Byte) - 64);
  until (not (GetTileEnum(Player.X, Player.Y, Deep) in StopTiles));  
  
  for FDeep := Low(TDeepEnum) to High(TDeepEnum) do
  begin
    IsBoss := False;
    for I := 0 to 255 do Mobs.Add(FDeep);
    for I := 0 to 255 do Items.Add(FDeep);
  end;
end;

function TMap.GetTile(ATileEnum: TTileEnum): TTile;
begin
  Result := Tile[ATileEnum][Deep];
end;

function TMap.GetTile(AX, AY: Byte): TTile;
begin
  Result := Tile[FMap[AX][AY][Deep]][Deep];
end;

function TMap.GetName: string;
begin
  Result := DeepName[Deep];
end;

function TMap.GetTileEnum(AX, AY: Byte; ADeep: TDeepEnum): TTileEnum;
begin
  Result := FMap[AX][AY][ADeep];
end;

procedure TMap.SetTileEnum(AX, AY: Byte; ADeep: TDeepEnum; ATileEnum: TTileEnum);
begin
  FMap[AX][AY][ADeep] := ATileEnum;
end;

function TMap.GetFog(AX, AY: Byte): Boolean;
begin
  Result := FFog[AX][AY][Deep];
end;

procedure TMap.SetFog(AX, AY: Byte; AFlag: Boolean);
begin
  FFog[AX][AY][Deep] := AFlag;
end;

function TMap.InMap(AX, AY: Integer): Boolean;
begin
  Result := (AX >= 0) and (AY >= 0) and (AX <= High(Byte)) and (AY <= High(Byte))
end;

function TMap.InView(AX, AY: Integer): Boolean;
var
  PX, PY: Integer;
begin
  PX := View.Width div 2;
  PY := View.Height div 2;
  Result := (AX >= Player.X - PX) and (AY >= Player.Y - PY) and (AX <= Player.X + PX - 1) and (AY <= Player.Y + PY - 1);
end;

function TMap.GetFOV(AX, AY: Byte): Boolean;
begin
  Result := FFOV[AX][AY];
end;

procedure TMap.SetFOV(AX, AY: Byte; AFlag: Boolean);
begin
  FFOV[AX][AY] := AFlag;
end;

initialization
  Map := TMap.Create;
  Map.Deep := deDarkWood;

finalization
  Map.Free;
  Map := nil;

end.
