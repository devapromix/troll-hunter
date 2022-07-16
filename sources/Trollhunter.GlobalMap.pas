unit Trollhunter.GlobalMap;

interface

uses
  Classes,
  Trollhunter.CustomMap,
  Trollhunter.Town,
  Trollhunter.Utils;

type
  TGlobalMap = class(TCustomMap)
  private
    FF: TStringList;
    FMap: array [0 .. MapSide - 1, 0 .. MapSide - 1] of Byte;
    FTowns: TTowns;
    function GetText: string;
    procedure SetText(const Value: string);
    function GetLocalMapLevel(const X, Y: Integer): Integer;
  public
    procedure Gen;
    procedure Clear; override;
    procedure Render; override;
    constructor Create;
    destructor Destroy; override;
    procedure Save;
    procedure Load;
    property Text: string read GetText write SetText;
    property Towns: TTowns read FTowns write FTowns;
  end;

implementation

{ TGlobalMap }

uses
  SysUtils,
  Trollhunter.Error;

const
  MapMinLevel = 0;
  MapMaxLevel = 20;
  MapsPerLevel = 3;
  FinalMapLevel = 25;

procedure TGlobalMap.Clear;
var
  X, Y: Integer;
begin
  for Y := 0 to Self.Height - 1 do
    for X := 0 to Self.Width - 1 do
      FMap[X][Y] := 0;
  Towns.Clear;
end;

constructor TGlobalMap.Create;
begin
  inherited;
  FF := TStringList.Create;
  Towns := TTowns.Create;
  Self.Clear;
end;

destructor TGlobalMap.Destroy;
begin
  Towns.Free;
  FF.Free;
  inherited;
end;

procedure TGlobalMap.Gen;
var
  I, J, X, Y: Integer;
begin
  try
    for I := 1 to MapsPerLevel do
      for J := 1 to MapMaxLevel do
      begin
        repeat
          X := Rand(0, Self.Width - 1);
          Y := Rand(0, Self.Height - 1);
        until (GetLocalMapLevel(X, Y) = J);
        FMap[X][Y] := J;
        // Final dungeon
        if (I = MapsPerLevel) and (J = MapMaxLevel) then
          FMap[X][Y] := FinalMapLevel;
      end;
    // Towns
    Towns.Gen;
    FMap[Self.Width div 2][Self.Height div 2] := 0;
  except
    on E: Exception do
      Error.Add('GlobalMap.Gen', E.Message);
  end;
end;

function TGlobalMap.GetText: string;
begin
  Self.Save;
  Result := FF.Text;
end;

function TGlobalMap.GetLocalMapLevel(const X, Y: Integer): Integer;
const
  V = MapSide div 2;
var
  S, PX, PY: Integer;
begin
  Result := 0;
  PX := ABS(X - V);
  PY := ABS(Y - V);
  S := Max(PX, PY);
  Result := S div 3;
  Result := Clamp(Result, MapMinLevel, MapMaxLevel);
end;

procedure TGlobalMap.Load;
var
  X, Y: Integer;
begin
  Self.Clear;
  try
    if (FF.Count > 0) then
      for Y := 0 to Self.Height - 1 do
        if Y < FF.Count then
          for X := 0 to Self.Width - 1 do
            FMap[X][Y] := Ord(FF[Y][X]) - (Ord('a') - 1);
  except
    on E: Exception do
      Error.Add('GlobalMap.Load', E.Message);
  end;
end;

procedure TGlobalMap.Render;
begin

end;

procedure TGlobalMap.Save;
var
  X, Y: Integer;
  S: string;
begin
  try
    FF.Clear;
    for Y := 0 to Self.Height - 1 do
    begin
      S := '';
      for X := 0 to Self.Width - 1 do
        S := S + Chr((Ord('a') - 1) + FMap[X][Y]);
      FF.Append(S);
    end;
  except
    on E: Exception do
      Error.Add('GlobalMap.Save', E.Message);
  end;
end;

procedure TGlobalMap.SetText(const Value: string);
begin
  FF.Text := Value;
  Self.Load;
end;

end.
