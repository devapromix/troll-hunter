unit uScreenshot;

interface

function TakeScreenshot(F: Boolean = True): string;

implementation

uses
  Windows,
  SysUtils,
  Trollhunter.Utils,
  PNGImage,
  Trollhunter.Graph,
  Trollhunter.Creatures,
  uError;

function TakeScreenshot(F: Boolean = True): string;
var
  T: TSystemTime;

  procedure Screenshot(FileName: string);
  var
    P: TPNGObject;
  begin
    try
      P := TPNGObject.Create;
      try
        P.Assign(Graph.Surface);
        P.SaveToFile(FileName);
      finally
        P.Free;
      end;
    except
      on E: Exception do
        Error.Add('uScreenshot.Screenshot', E.Message);
    end;
  end;

begin
  if ParamDebug then
    Exit;
  try
    GetSystemTime(T);
    if F then
      Result := IntToStr(T.wYear) + IntToStr(T.wMonth) + IntToStr(T.wDay) +
        IntToStr(T.wHour) + IntToStr(T.wMinute) + IntToStr(T.wSecond) + '.png'
    else if (Trim(Creatures.PC.Name) <> '') then
    begin
      Screenshot('Last.png');
      Result := Creatures.PC.Name + '.png';
    end;
    Screenshot(Result);
  except
    on E: Exception do
      Error.Add('uScreenshot.TakeScreenshot', E.Message);
  end;
end;

end.
