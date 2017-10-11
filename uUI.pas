unit uUI;

interface

uses uMsgLog;

type
  TIconEnum = (ic);

type
  UI = class(TObject)
    class procedure Bar(X, LM, Y, Wd: Byte; Cur, Max: Word; AColor, DarkColor: Cardinal);
    class procedure Title(S: string; AY: Byte = 1; BGColor: Cardinal = 0);
    class procedure FromAToZ(const Max: Byte = 0);
    class function KeyToStr(AKey: string; AStr: string = ''; AColor: string = 'Key'): string;
    class function GoldLeft(Value: Word): string;
    class function Icon(AIcon: TIconEnum; AColor: string = ''): string;
end;

implementation

uses
  SysUtils, BearLibTerminal, uTerminal, uGame, GNUGetText;

const
  F = '[[%s]]';
  IconStr: array[TIconEnum]of string = ('');

{ TMyClass }

class procedure UI.Bar(X, LM, Y, Wd: Byte; Cur, Max: Word; AColor,
  DarkColor: Cardinal);
var
  I, L, W: Byte;
begin
  L := Wd;
  W := Round(Cur / Max * L);
  for I := 0 to L do
  begin
    Terminal.BackgroundColor(DarkColor);
    if ((I <= W) and (Cur > 0)) then
      Terminal.BackgroundColor(AColor);
    Terminal.Print(X + I + LM, Y, ' ');
    Terminal.BackgroundColor(0); // Clear background
  end;
end;

class procedure UI.FromAToZ(const Max: Byte = 0);
var
  I: Char;
  J: Byte;
begin
  if Game.Wizard then
    for I := 'A' to 'Z' do
      Terminal.Print(1, (Ord(I) - Ord('A')) + 2, Format(F, [I]), clGray)
  else
    if (Max > 0) then
      for J := 1 to Max do
      Terminal.Print(1, J + 1, Format(F, [Chr(J + Ord('A') - 1)]), clDarkGray);

end;

class function UI.GoldLeft(Value: Word): string;
begin
  Result := Format(F, [Format(Terminal.Icon('F8D5')
    + _('%d gold left'), [Value])]);
end;

class function UI.Icon(AIcon: TIconEnum; AColor: string): string;
var
  ANum: string;
begin
  if (AColor = '') then
  Result := Format('[font=icon][U+%s][/font]',
    [UpperCase(ANum)])
      else Result := Format('[font=icon][color=%s][U+%s][/color][/font]',
    [LowerCase(AColor), UpperCase(ANum)]);
end;

class function UI.KeyToStr(AKey, AStr, AColor: string): string;
begin
  Result := Trim(Terminal.Colorize(Format(F, [UpperCase(AKey)]),
    Terminal.GetColorFromIni(AColor)) + ' ' + AStr);
end;

class procedure UI.Title(S: string; AY: Byte = 1; BGColor: Cardinal = 0);
var
  X: Byte;
begin
  if (BGColor > 0) then
  begin
    Terminal.BackgroundColor(BGColor);
    Terminal.Clear;
  end;
  X := Terminal.Window.Width div 2;
  Terminal.ForegroundColor(Terminal.GetColorFromIni('Title', 'Yellow'));
  Terminal.Print(X, AY, Format(FT, [S]), TK_ALIGN_CENTER);
  Terminal.ForegroundColor(clDefault);
end;

end.
