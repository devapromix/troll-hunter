unit uCommon;

interface

type
  TEntSize = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

const
  Version = '0.2';

const
  clDarkGray  = $FF222222;
  clDarkRed   = $FF880000;
  clDarkGreen = $FF008800;
  clDarkBlue  = $FF000088;
  clYellow    = $FFFFFF00;

var
  Screen, Panel, View, Status, Log: TEntSize;

function SetEntSize(ALeft, ATop, AWidth, AHeight: Byte): TEntSize;
function Clamp(Value, AMin, AMax: Integer; Flag: Boolean = True): Integer;
function Percent(N, P: Integer): Integer;
function BarWidth(CX, MX, WX: Integer): Integer;

implementation

function SetEntSize(ALeft, ATop, AWidth, AHeight: Byte): TEntSize;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

function Clamp(Value, AMin, AMax: Integer; Flag: Boolean = True): Integer;
begin
  Result := Value;
  if (Result < AMin) then
    if Flag then
      Result := AMin
    else
      Result := AMax;
  if (Result > AMax) then
    if Flag then
      Result := AMax
    else
      Result := AMin;
end;

function Percent(N, P: Integer): Integer;
begin
  Result := N * P div 100;
end;

function BarWidth(CX, MX, WX: Integer): Integer;
begin
  Result := Round(CX / MX * WX);
end;

end.

