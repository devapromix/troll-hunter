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
  Version = '0.1';

var
  Screen, Panel, View, Status, Log: TEntSize;

function SetEntSize(ALeft, ATop, AWidth, AHeight: Byte): TEntSize;

implementation

function SetEntSize(ALeft, ATop, AWidth, AHeight: Byte): TEntSize;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

end.

