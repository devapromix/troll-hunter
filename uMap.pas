unit uMap;

interface

type
  TMapEnum = (deDarkWood, deGrayCave, deDeepCave, deBloodCave, deDungeonOfDoom);

const
  FinalDungeon = deDungeonOfDoom;

type
  TTile = record
    Symbol: Char;
    Name: string;
    Color: Cardinal;
  end;

type
  TTileEnum = (teDefaultFloor, teDefaultWall, teRock, teFloor1, teFloor2,
    teFloor3, teUpStairs, teDnStairs, teWater);

const
  StopTiles = [teDefaultWall];
  FreeTiles = [teDefaultFloor, teRock, teFloor1, teFloor2, teFloor3, teUpStairs,
    teDnStairs, teWater];
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

uses SysUtils, Math, uPlayer, uMob, uItem, GNUGetText, uTerminal;

{ TMap }

procedure TMap.InitTiles;
begin
  // DefaultFloor
  AddTile('"', _('Grass'), $FF113311, teDefaultFloor, deDarkWood);
  AddTile(':', _('Dirt'), $FF331133, teDefaultFloor, deGrayCave);
  AddTile('.', _('Stone'), $FF222111, teDefaultFloor, deDeepCave);
  AddTile(';', _('Stone'), $FF330000, teDefaultFloor, deBloodCave);
  AddTile('~', _('Stone'), $FF002200, teDefaultFloor, deDungeonOfDoom);
  // DefaultWall
  AddTile('T', _('Tree'), $FF006622, teDefaultWall, deDarkWood);
  AddTile('#', _('Wall'), $FF444422, teDefaultWall, deGrayCave);
  AddTile('#', _('Wall'), $FF222133, teDefaultWall, deDeepCave);
  AddTile('#', _('Wall'), $FF322118, teDefaultWall, deBloodCave);
  AddTile('#', _('Wall'), $FF112211, teDefaultWall, deDungeonOfDoom);
  // Rock
  AddTile('^', _('Rock'), $FF556655, teRock, deDarkWood);
  AddTile(':', _('Wall'), $FF444422, teRock, deGrayCave);
  AddTile(':', _('Wall'), $FF222133, teRock, deDeepCave);
  AddTile(':', _('Wall'), $FF322118, teRock, deBloodCave);
  AddTile(':', _('Wall'), $FF112233, teRock, deDungeonOfDoom);
  // Floor #1
  AddTile('"', _('Grass'), $FF446644, teFloor1, deDarkWood);
  AddTile('"', _('Grass'), $FF99AA99, teFloor1, deGrayCave);
  AddTile('.', _('Stone'), $FF224422, teFloor1, deDeepCave);
  AddTile(';', _('Stone'), $FF225533, teFloor1, deBloodCave);
  AddTile('~', _('Stone'), $FF228833, teFloor1, deDungeonOfDoom);
  // Floor #2
  AddTile('"', _('Grass'), $FF447755, teFloor2, deDarkWood);
  AddTile('"', _('Grass'), $FF779977, teFloor2, deGrayCave);
  AddTile('.', _('Stone'), $FF22CC44, teFloor2, deDeepCave);
  AddTile(';', _('Stone'), $FF333322, teFloor2, deBloodCave);
  AddTile('~', _('Stone'), $FF334422, teFloor2, deDungeonOfDoom);
  // Floor #3
  AddTile('"', _('Grass'), $FF778866, teFloor3, deDarkWood);
  AddTile('"', _('Grass'), $FF668866, teFloor3, deGrayCave);
  AddTile('.', _('Stone'), $FF338855, teFloor3, deDeepCave);
  AddTile(';', _('Stone'), $FF223333, teFloor3, deBloodCave);
  AddTile('~', _('Stone'), $FF226622, teFloor3, deDungeonOfDoom);
  // Up Stairs
  AddTile('*', _('Stairs'), $FFFFFF00, teUpStairs, deDarkWood);
  AddTile('<', _('Stairs'), $FFEEEE00, teUpStairs, deGrayCave);
  AddTile('<', _('Stairs'), $FFDDDD00, teUpStairs, deDeepCave);
  AddTile('<', _('Stairs'), $FFCCCC00, teUpStairs, deBloodCave);
  AddTile('<', _('Stairs'), $FFBBBB00, teUpStairs, deDungeonOfDoom);
  // Down Stairs
  AddTile('*', _('Stairs'), $FFFFFF00, teDnStairs, deDarkWood);
  AddTile('>', _('Stairs'), $FFEEEE00, teDnStairs, deGrayCave);
  AddTile('>', _('Stairs'), $FFDDDD00, teDnStairs, deDeepCave);
  AddTile('>', _('Stairs'), $FFCCCC00, teDnStairs, deBloodCave);
  AddTile('>', _('Stairs'), $FFBBBB00, teDnStairs, deDungeonOfDoom);
  // Water
  AddTile('=', _('Water'), $FF333388, teWater, deDarkWood);
  AddTile('=', _('Water'), $FF333377, teWater, deGrayCave);
  AddTile('=', _('Water'), $FF222266, teWater, deDeepCave);
  AddTile('=', _('Water'), $FF222255, teWater, deBloodCave);
  AddTile('=', _('Water'), $FF222244, teWater, deDungeonOfDoom);
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
      FFOV[EnsureRange(X, 0, High(Byte))][EnsureRange(Y, 0, High(Byte))] := False;
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
        end;
      deBloodCave:
        begin
          Self.Clear(Z, teDefaultWall);
          GenCave(3, 29, 2999);
        end;
      deDungeonOfDoom:
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
  repeat
    Player.X := RandomRange(64, High(Byte) - 64);
    Player.Y := RandomRange(64, High(Byte) - 64);
  until (not(GetTileEnum(Player.X, Player.Y, Current) in StopTiles));

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
  case Current of
    deDarkWood:
      Result := _('Dark Wood');
    deGrayCave:
      Result := _('Gray Cave');
    deDeepCave:
      Result := _('Deep Cave');
    deBloodCave:
      Result := _('Blood Cave');
    deDungeonOfDoom:
      Result := _('Dungeon Of Doom');
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

procedure TMap.SetTileEnum(AX, AY: Byte; AZ: TMapEnum;
  ATileEnum: TTileEnum);
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
