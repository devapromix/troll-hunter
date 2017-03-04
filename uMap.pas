unit uMap;

interface

uses uCommon;

type
  TDeepEnum = (deDarkWood, deGrayCave);

type
  TTile = record
    Symbol: Char;
    Name: string;
    Color: Cardinal;
  end;

type
  TTileEnum = (teFloor);

const
  Tile: array[TTileEnum, TDeepEnum] of TTile = (
  // Dark Wood
  ((Symbol: '"'; Name: 'Grass'; Color: clDarkGreen;),
  // Gray Cave
   (Symbol: '.'; Name: 'Stone'; Color: clDarkGray;))
  );

type
  TMap = class(TObject)
  private
    FDeep: TDeepEnum;
    FMap: array [Byte, Byte, TDeepEnum] of TTileEnum;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    //procedure Gen;
    property Deep: TDeepEnum read FDeep write FDeep;
    function GetTile(X, Y: Byte): TTile;
    function GetName: string;
  end;

var
  Map: TMap = nil;

implementation

const
  DeepName: array [TDeepEnum] of string = ('Dark Wood', 'Gray Cave');

{ TMap }

procedure TMap.Clear;
var
  X, Y: Byte;
  I: TDeepEnum;
begin
  for I := Low(TDeepEnum) to High(TDeepEnum) do
  begin
    for Y := 0 to High(Byte) do
      for X := 0 to High(Byte) do
        FMap[X][Y][I] := teFloor;
  end;
end;

constructor TMap.Create;
begin
  Self.Clear;
end;

destructor TMap.Destroy;
begin

  inherited;
end;

function TMap.GetName: string;
begin
  Result := DeepName[Deep];
end;

function TMap.GetTile(X, Y: Byte): TTile;
begin
  Result := Tile[FMap[X][Y][Deep]][Deep];
end;

initialization
  Map := TMap.Create;
  Map.Deep := deDarkWood; //deGrayCave;

finalization
  Map.Free;
  Map := nil;

end.
