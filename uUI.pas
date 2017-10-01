unit uUI;

interface

uses uMsgLog;

type
  UI = class(TObject)
    class procedure Bar(X, LM, Y, Wd: Byte; Cur, Max: Word; AColor, DarkColor: Cardinal);
    class procedure Title(S: string; AY: Byte = 1);
    class procedure FromAToZ;
    class function KeyToStr(AKey: string; AStr: string = ''): string;
    class function GoldLeft(V: Word): string;
end;

implementation

uses
  SysUtils, BearLibTerminal, uTerminal, uGame, GNUGetText;

const
  F = '[[%s]]';

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

class procedure UI.FromAToZ;
var
  I: Char;
begin
  if Game.Wizard then
    for I := 'A' to 'Z' do
      Terminal.Print(1, (Ord(I) - Ord('A')) + 2, Format(F, [I]), clGray);
end;

class function UI.GoldLeft(V: Word): string;
begin
  Result := Format(F, [Format(Terminal.Icon('F8D5')
    + _('%d gold left'), [V])]);
end;

class function UI.KeyToStr(AKey, AStr: string): string;
begin
  Result := Trim(Terminal.Colorize(Format(F, [UpperCase(AKey)]),
    Terminal.GetColorFromIni('Key')) + ' ' + AStr);
end;

class procedure UI.Title(S: string; AY: Byte);
var
  X: Byte;
begin
  X := Terminal.Window.Width div 2;
  Terminal.ForegroundColor(Terminal.GetColorFromIni('Title', 'Yellow'));
  Terminal.Print(X, AY, Format(FT, [S]), TK_ALIGN_CENTER);
  Terminal.ForegroundColor(clDefault);
end;

end.
