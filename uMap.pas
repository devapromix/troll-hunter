unit uMap;

interface

uses uCommon;

type
  TDeepEnum = (deDarkWood, deGrayCave, deDeepCave);

const
  DeepName: array [TDeepEnum] of string = (
    'Dark Wood', 'Gray Cave', 'Deep Cave');

type
  TTile = record
    Symbol: Char;
    Name: string;
    Color: Cardinal;
  end;

type
  TTileEnum = (teDefaultFloor, teDefaultWall,
    teFloor1, teFloor2, teFloor3,
    teUpStairs, teDnStairs);

const
  Tile: array[TTileEnum, TDeepEnum] of TTile = (
  ( // DefaultFloor
    (Symbol: '"'; Name: 'Grass'; Color: clDarkGreen;), // Dark Wood
    (Symbol: ':'; Name: 'Dirt'; Color: clDarkGray;),   // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: clDarkGray;)   // Deep Cave
  ),
  ( // DefaultWall
    (Symbol: 'T'; Name: 'Tree'; Color: $FF006622;), // Dark Wood
    (Symbol: '#'; Name: 'Wall'; Color: clDarkGray;),   // Gray Cave
    (Symbol: '#'; Name: 'Wall'; Color: clDarkGray;)   // Deep Cave
  ),
  ( // Floor1
    (Symbol: '"'; Name: 'Grass'; Color: $FF222222;), // Dark Wood
    (Symbol: '.'; Name: 'Stone'; Color: clDarkGray;),   // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: clDarkGray;)   // Deep Cave
  ),
  ( // Floor2
    (Symbol: '"'; Name: 'Grass'; Color: $FF222222;), // Dark Wood
    (Symbol: '.'; Name: 'Stone'; Color: clDarkGray;),   // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: clDarkGray;)   // Deep Cave
  ),
  ( // Floor3
    (Symbol: '"'; Name: 'Grass'; Color: $FF222222;), // Dark Wood
    (Symbol: '.'; Name: 'Stone'; Color: clDarkGray;),   // Gray Cave
    (Symbol: '.'; Name: 'Stone'; Color: clDarkGray;)   // Deep Cave
  ),
  ( // UpStairs
    (Symbol: '<'; Name: 'Grass'; Color: clYellow;), // Dark Wood
    (Symbol: '<'; Name: 'Stone'; Color: clYellow;),   // Gray Cave
    (Symbol: '<'; Name: 'Stone'; Color: clYellow;)   // Deep Cave
  ),
  ( // DnStairs
    (Symbol: '>'; Name: 'Grass'; Color: clDarkRed;), // Dark Wood
    (Symbol: '>'; Name: 'Stone'; Color: clDarkRed;),   // Gray Cave
    (Symbol: '>'; Name: 'Stone'; Color: clDarkRed;)   // Deep Cave
  )
  );

type
  TMap = class(TObject)
  private
    FDeep: TDeepEnum;
    FMap: array [Byte, Byte, TDeepEnum] of TTileEnum;
    procedure AddPart(AX, AY: Byte; ADeep: TDeepEnum; AType: Byte; ADen: Word; ATileEnum: TTileEnum);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear(ADeep: TDeepEnum; ATileEnum: TTileEnum);
    procedure Gen;
    property Deep: TDeepEnum read FDeep write FDeep;
    function GetTile(X, Y: Byte): TTile;
    procedure SetTileEnum(X, Y: Byte; ADeep: TDeepEnum; ATileEnum: TTileEnum);
    function GetTileEnum(X, Y: Byte; ADeep: TDeepEnum): TTileEnum;
    function GetName: string;
  end;

var
  Map: TMap = nil;

implementation

uses Math;

{ TMap }

procedure TMap.AddPart(AX, AY: Byte; ADeep: TDeepEnum; AType: Byte; ADen: Word; ATileEnum: TTileEnum);
var
  K: Word;
  X, Y: Byte;
begin
  X := AX;
  Y := AY;
  AType := Clamp(AType, 2, 9);
  for K := 0 to ADen do
  begin
    if (Round(Random(AType)) = 1) and (X > 0) then
    begin
      X := X - 1;
      //if (GetTileEnum(X, Y) <> teDefault) then Continue;
      SetTileEnum(X, Y, ADeep, ATileEnum);
    end;
    if (Round(Random(AType)) = 1) and (X < High(Byte)) then
    begin
      X := X + 1;
      //if (GetTileEnum(X, Y) <> teDefault) then Continue;
      SetTileEnum(X, Y, ADeep, ATileEnum);
    end;
    if (Round(Random(AType)) = 1) and (Y > 0) then
    begin
      Y := Y - 1;
      //if (GetTileEnum(X, Y) <> teDefault) then Continue;
      SetTileEnum(X, Y, ADeep, ATileEnum);
    end;
    if (Round(Random(AType)) = 1) and (Y < High(Byte)) then
    begin
      Y := Y + 1;
      //if (GetTileEnum(X, Y) <> teDefault) then Continue;
      SetTileEnum(X, Y, ADeep, ATileEnum);
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
        for I := 0 to 19 do
        begin
          repeat
            X := Math.RandomRange(10, High(Byte) - 10);
            Y := Math.RandomRange(10, High(Byte) - 10);
          until(GetTileEnum(X, Y, pred(FDeep)) = teDefaultFloor);
          Self.AddPart(X, Y, FDeep, 9, 2999, teDefaultFloor);
          SetTileEnum(X, Y, pred(FDeep), teDnStairs);
          SetTileEnum(X, Y, FDeep, teUpStairs);
        end;
      end;
      deDeepCave:
      begin
        Self.Clear(FDeep, teDefaultWall);
        for I := 0 to 49 do
        begin
          repeat
            X := Math.RandomRange(10, High(Byte) - 10);
            Y := Math.RandomRange(10, High(Byte) - 10);
          until(GetTileEnum(X, Y, pred(FDeep)) = teDefaultFloor);
          Self.AddPart(X, Y, FDeep, 2, 999, teDefaultFloor);
          SetTileEnum(X, Y, pred(FDeep), teDnStairs);
          SetTileEnum(X, Y, FDeep, teUpStairs);
        end;
      end;
    end;
  end;
end;

function TMap.GetName: string;
begin
  Result := DeepName[Deep];
end;

function TMap.GetTile(X, Y: Byte): TTile;
begin
  Result := Tile[FMap[X][Y][Deep]][Deep];
end;

function TMap.GetTileEnum(X, Y: Byte; ADeep: TDeepEnum): TTileEnum;
begin
  Result := FMap[X][Y][ADeep];
end;

procedure TMap.SetTileEnum(X, Y: Byte; ADeep: TDeepEnum; ATileEnum: TTileEnum);
begin
  FMap[X][Y][ADeep] := ATileEnum;
end;

initialization
  Map := TMap.Create;
  Map.Deep := deDarkWood;

finalization
  Map.Free;
  Map := nil;

end.
