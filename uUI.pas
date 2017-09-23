unit uUI;

interface

type
  UI = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
    class function KeyToStr(AKey: string; AStr: string = ''): string;
    class procedure Bar(X, LM, Y, Wd: Byte; Cur, Max: Word;
      AColor, DarkColor: Cardinal);
    class procedure Title(S: string; AY: Byte = 1);
    class procedure FromAToZ;
    class function GoldLeft(V: Word): string;  
end;

implementation

uses
  SysUtils, BearLibTerminal, uTerminal, uGame;

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
    if (I <= W) and (Cur > 0) then
      Terminal.BackgroundColor(AColor);
    Terminal.Print(X + I + LM, Y, ' ');
    Terminal.BackgroundColor(0); // Clear background
  end;
end;

constructor UI.Create;
begin
  inherited;

end;

destructor UI.Destroy;
begin

  inherited;
end;

class procedure UI.FromAToZ;
var
  I: Char;
begin
  if Game.Wizard then
    for I := 'A' to 'Z' do
    begin
      Terminal.ForegroundColor(clGray);
      Terminal.Print(1, Ord(I) + 2, '[[' + I + ']]', TK_ALIGN_LEFT);
    end;
end;

class function UI.GoldLeft(V: Word): string;
begin

end;

class function UI.KeyToStr(AKey, AStr: string): string;
begin
  Result := Trim(Terminal.Colorize(Format('[[%s]]', [UpperCase(AKey)]),
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
