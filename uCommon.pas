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
  Version = '0.4';

const
  CharacterDumpFileName = 'trollhunter-character-dump.txt';
  HelpFileName          = 'trollhunter-help.txt';

const
  clDarkGray  = $FF222222;
  clDarkRed   = $FF880000;
  clDarkGreen = $FF008800;
  clDarkBlue  = $FF000088;
  clYellow    = $FFFFFF00;

const
  clFog       = $FF111111;

var
  Screen, Panel, View, Status, Log, Info: TEntSize;
  WizardMode: Boolean = False;
  GameMode: Boolean = False;
  CanClose: Boolean = False;

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

implementation

uses SysUtils;

procedure Init;
var
  I: Byte;
begin
  Randomize;
  for I := 1 to ParamCount do
  begin
    if (LowerCase(ParamStr(I)) = '-w') then WizardMode := True;

  end;
end;

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
  if (S[1] in ['a', 'e', 'i', 'o', 'u']) then
    Result := 'an ' + S else
      Result := 'a ' + S;
end;

function GetDescThe(S: string): string;
begin
  Result := 'the ' + S;
end;

function GetDescSig(V: Integer): string;
begin
  if (V > 0) then
    Result := '+' + IntToStr(V) else
      Result := IntToStr(V);
end;

function GetDateTime(DateSep: Char = '.'; TimeSep: Char = ':'): string;
begin
  SysUtils.DateSeparator := DateSep;
  SysUtils.TimeSeparator := TimeSep;
  Result := DateToStr(Date) + '-' + TimeToStr(Time);
end;

initialization
  Init();

end.

