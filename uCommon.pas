unit uCommon;

interface

type
  TEntSize = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

type
  TDamage = record
    Min: Word;
    Max: Word;
  end;

const
  Version = '0.8';

const
  FT = '== %s ==';

const
  clRed = $FFFF0000;
  clGreen = $FF00FF00;
  clBlue = $FF0000FF;
  clDarkGray = $FF222222;
  clDarkRed = $FF880000;
  clDarkGreen = $FF008800;
  clDarkBlue = $FF000088;
  clYellow = $FFFFFF00;
  clGray = $FF777777;
  clWhite = $FFDDDDDD;

const
  clFog = $FF111111;

var
  clDefault:    Cardinal = $FFFFFF00;
  clBackground: Cardinal = $00000000;

var
  Screen, Panel, View, Status, Log, Info: TEntSize;

function SetEntSize(ALeft, ATop, AWidth, AHeight: Byte): TEntSize;
function Clamp(Value, AMin, AMax: Integer; Flag: Boolean = True): Integer;
function Percent(N, P: Integer): Integer;
function BarWidth(CX, MX, WX: Integer): Integer;
function GetDist(X1, Y1, X2, Y2: Single): Word;
function GetCapit(S: string): string;
function GetDescAn(S: string): string;
function GetDescThe(S: string): string;
function GetDescSig(V: Integer): string;
function GetDateTime(DateSep: Char = '.'; TimeSep: Char = ':'): string;
function GetTextScreenshot: string;
function GetPureText(const S: string): string;

implementation

uses SysUtils, Classes, Math, uTerminal, gnugettext, BearLibTerminal;

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

function GetDist(X1, Y1, X2, Y2: Single): Word;
begin
  Result := Round(sqrt(sqr(X2 - X1) + sqr(Y2 - Y1)));
end;

function GetCapit(S: string): string;
begin
  Result := UpCase(S[1]) + Copy(S, 2, Length(S));
end;

function GetDescAn(S: string): string;
begin
  Result := LowerCase(S);
  if (GetCurrentLanguage <> 'en') then
    Exit;
  if (S[1] in ['a', 'e', 'i', 'o', 'u']) then
    Result := 'an ' + Result
  else
    Result := 'a ' + Result;
end;

function GetDescThe(S: string): string;
begin
  Result := LowerCase(S);
  if (GetCurrentLanguage <> 'en') then
    Exit;
  Result := 'the ' + Result;
end;

function GetDescSig(V: Integer): string;
begin
  if (V > 0) then
    Result := '+' + IntToStr(V)
  else
    Result := IntToStr(V);
end;

function GetDateTime(DateSep: Char = '.'; TimeSep: Char = ':'): string;
begin
  Result := DateToStr(Date) + '-' + TimeToStr(Time);
  Result := StringReplace(Result, '.', DateSep, [rfReplaceAll]);
  Result := StringReplace(Result, ':', TimeSep, [rfReplaceAll]);
end;

function GetTextScreenshot: string;
var
  SL: TStringList;
  X, Y, C: Byte;
  S: string;
begin
  SL := TStringList.Create;
  try
    for Y := 0 to View.Height - 1 do
    begin
      S := '';
      for X := 0 to View.Width - 1 do
      begin
        C := Terminal.Pick(X, Y);
        if (C >= 32) and (C < 126) then
          S := S + Chr(C)
        else
          S := S + ' ';
      end;
      SL.Append(S);
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function GetPureText(const S: string): string;
var
  I: Integer;
  B: Boolean;
begin
  B := True;
  Result := '';
  for I := 1 to Length(S) do
  begin
    if (S[I] = '[') then B := False;
    if B then Result := Result + S[I];
    if (S[I] = ']') then B := True;
  end;
end;

end.
