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
    teUpStairs, teDnStairs);

const
  Tile: array[TTileEnum, TDeepEnum] of TTile = (
  ( // DefaultFloor
    (Symbol: '"'; Name: 'Grass'; Color: $FF113311;), // Dark Wood
    (Symbol: ':'; Name: 'Dirt';  Color: $FF331133;),   // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222111;),   // Deep Cave
    (Symbol: ';'; Name: 'Stone'; Color: $FF330000;),   // Blood Cave
    (Symbol: '~'; Name: 'Stone'; Color: $FF002200;)   // Dungeon of Doom
  ),
  ( // DefaultWall
    (Symbol: 'T'; Name: 'Tree';  Color: $FF006622;), // Dark Wood
    (Symbol: '#'; Name: 'Wall';  Color: $FF444422;),   // Gray Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF222133;),   // Deep Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF322118;),   // Blood Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF112211;)   // Dungeon of Doom
  ),
  ( // Rock
    (Symbol: '^'; Name: 'Rock';  Color: $FF556655;), // Dark Wood
    (Symbol: '#'; Name: 'Wall';  Color: $FF444422;),   // Gray Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF222133;),   // Deep Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF322118;),   // Blood Cave
    (Symbol: '#'; Name: 'Wall';  Color: $FF322118;)   // Dungeon of Doom
  ),
  ( // Floor1
    (Symbol: '"'; Name: 'Grass'; Color: $FF222222;), // Dark Wood
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Deep Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Blood Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;)   // Dungeon of Doom
  ),
  ( // Floor2
    (Symbol: '"'; Name: 'Grass'; Color: $FF222222;), // Dark Wood
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Deep Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Blood Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;)   // Dungeon of Doom
  ),
  ( // Floor3
    (Symbol: '"'; Name: 'Grass'; Color: $FF222222;), // Dark Wood
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Deep Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;),   // Blood Cave
    (Symbol: '.'; Name: 'Stone'; Color: $FF222222;)   // Dungeon of Doom
  ),
  ( // UpStairs
    (Symbol: '*'; Name: 'Grass'; Color: $FFFFFF00;), // Dark Wood
    (Symbol: '<'; Name: 'Stone'; Color: $FFFFFF00;),   // Gray Cave
    (Symbol: '<'; Name: 'Stone'; Color: $FFFFFF00;),   // Deep Cave
    (Symbol: '<'; Name: 'Stone'; Color: $FFFFFF00;),   // Blood Cave
    (Symbol: '<'; Name: 'Stone'; Color: $FFFFFF00;)   // Dungeon of Doom
  ),
  ( // DnStairs
    (Symbol: '*'; Name: 'Grass'; Color: $FFFFFF00;), // Dark Wood
    (Symbol: '>'; Name: 'Stone'; Color: $FFFFFF00;),   // Gray Cave
    (Symbol: '>'; Name: 'Stone'; Color: $FFFFFF00;),   // Deep Cave
    (Symbol: '>'; Name: 'Stone'; Color: $FFFFFF00;),   // Blood Cave
    (Symbol: '>'; Name: 'Stone'; Color: $FFFFFF00;)   // Dungeon of Doom
  )
  );

type
  TMap = class(TObject)
  private
    FDeep: TDeepEnum;
    FMap: array [Byte, Byte, TDeepEnum] of TTileEnum;
    procedure AddPart(AX, AY: Byte; ADeep: TDeepEnum; AType: Byte; ADen: Word; ABaseTileEnum, ATileEnum: TTileEnum);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear(ADeep: TDeepEnum; ATileEnum: TTileEnum);
    procedure Gen;
    property Deep: TDeepEnum read FDeep write FDeep;
    function GetTile(AX, AY: Byte): TTile;
    procedure SetTileEnum(AX, AY: Byte; ADeep: TDeepEnum; ATileEnum: TTileEnum);
    function GetTileEnum(AX, AY: Byte; ADeep: TDeepEnum): TTileEnum;
    function GetName: string;
  end;

var
  Map: TMap = nil;

implementation

uses Math;

{ TMap }

procedure TMap.AddPart(AX, AY: Byte; ADeep: TDeepEnum; AType: Byte; ADen: Word; ABaseTileEnum, ATileEnum: TTileEnum);
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

procedure TMap.Clear(ADeep: TDeepEnum; ATileEnum: TTileEnum);
var
  X, Y: Byte;
begin
  for Y := 0 to High(Byte) do
    for X := 0 to High(Byte) do
      FMap[X][Y][ADeep] := ATileEnum;
end;

constructor TMap.Create;
begin
  Self.Gen;
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
      Self.AddPart(X, Y, FDeep, D, V, teDefaultWall, teDefaultFloor);
      SetTileEnum(X, Y, pred(FDeep), teDnStairs);
      SetTileEnum(X, Y, FDeep, teUpStairs);
    end;
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
  end;
end;

function TMap.GetName: string;
begin
  Result := DeepName[Deep];
end;

function TMap.GetTile(AX, AY: Byte): TTile;
begin
  Result := Tile[FMap[AX][AY][Deep]][Deep];
end;

function TMap.GetTileEnum(AX, AY: Byte; ADeep: TDeepEnum): TTileEnum;
begin
  Result := FMap[AX][AY][ADeep];
end;

procedure TMap.SetTileEnum(AX, AY: Byte; ADeep: TDeepEnum; ATileEnum: TTileEnum);
begin
  FMap[AX][AY][ADeep] := ATileEnum;
end;

initialization
  Map := TMap.Create;
  Map.Deep := deDarkWood;

finalization
  Map.Free;
  Map := nil;

end.
