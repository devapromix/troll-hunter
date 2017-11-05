unit uUI;

interface

uses uMsgLog;

type
  TIconEnum = (icMale, icFemale, icPlus, icMinus, icQuestion, icGold, icFlag,
    icLife, icMana, icStar, icHammer, icElixir, icShield, icFood, icLeaf,
    icFeather, icStr, icDex, icBook, icVision, icFire, icIce,
    icLightning, icDrop, icSword, icSun, icMoon, icTroph, icAst, icKey, icRepair,
    icBook2);

type
  UI = class(TObject)
    class procedure Bar(X, LM, Y, Wd: Byte; Cur, Max: Word;
      AColor, DarkColor: Cardinal);
    class procedure Title(const S: string; AY: Byte = 1; BGColor: Cardinal = 0);
    class procedure FromAToZ(const Max: Byte = 0);
    class procedure RenderTile(const S: string);
    class function KeyToStr(AKey: string; AStr: string = '';
      AColor: string = 'Key'): string;
    class function GoldLeft(const Value: Cardinal): string;
    class function Icon(const AIcon: TIconEnum; const AColor: string = ''): string;
  end;

implementation

uses
  SysUtils, BearLibTerminal, uTerminal, uGame, uLanguage;

const
  IconStr: array [TIconEnum] of string = ('F8D0','F8D1','F8D2','F8D3','F8D4',
  'F8D5','F8D6','F8D7','F8D8','F8D9','F8DA','F8DB','F8DC','F8DD','F8DE','F8DF',
  'F8E0','F8E1','F8E2','F8E3','F8E4','F8E5','F8E6','F8E7','F8E8','F8E9','F8EA',
  'F8EB','F8EC','F8ED','F8EE','F8EF');
  F = '[[%s]]';

  { UI }

class procedure UI.Bar(X, LM, Y, Wd: Byte; Cur, Max: Word;
  AColor, DarkColor: Cardinal);
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
    Terminal.BackgroundColor(0);
  end;
end;

class procedure UI.FromAToZ(const Max: Byte = 0);
var
  I: Char;
  J: Byte;
begin
  if Mode.Wizard then
    for I := 'A' to 'Z' do
      Terminal.Print(1, (Ord(I) - Ord('A')) + 2, Format(F, [I]), clGray, clBlack)
  else if (Max > 0) then
    for J := 1 to Max do
      Terminal.Print(1, J + 1, Format(F, [Chr(J + Ord('A') - 1)]), clDarkGray, clBlack);
end;

class function UI.GoldLeft(const Value: Cardinal): string;
begin
  Result := Format(F, [Format(UI.Icon(icGold) + _('%d gold left'), [Value])]);
end;

class function UI.Icon(const AIcon: TIconEnum; const AColor: string): string;
begin
  if (AColor = '') then
    Result := Format('[font=icon][U+%s][/font]', [UpperCase(IconStr[AIcon])])
  else
    Result := Format('[font=icon][color=%s][U+%s][/color][/font]',
      [LowerCase(AColor), UpperCase(IconStr[AIcon])]);
end;

class function UI.KeyToStr(AKey, AStr, AColor: string): string;
begin
  Result := Trim(Terminal.Colorize(Format(F, [UpperCase(AKey)]),
    Terminal.GetColorFromIni(AColor)) + ' ' + AStr);
end;

class procedure UI.RenderTile(const S: string);
begin
  Terminal.Print(0, 0, '[U+E000]');
end;

class procedure UI.Title(const S: string; AY: Byte = 1; BGColor: Cardinal = 0);
var
  GX: Byte;
begin
  if (BGColor > 0) then
  begin
    Terminal.BackgroundColor(BGColor);
    Terminal.Clear;
  end;
  GX := Terminal.Window.Width div 2;
  Terminal.ForegroundColor(Terminal.GetColorFromIni('Title', 'Yellow'));
  Terminal.Print(GX, AY, Format(FT, [S]), TK_ALIGN_CENTER);
  Terminal.ForegroundColor(clDefault);
end;

end.
