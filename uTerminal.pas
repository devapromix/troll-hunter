unit uTerminal;

interface

uses Types, BearLibTerminal;

type
  TSize = Types.TSize;

type
  TEntSize = record
    Left: Integer;
    Top: Integer;
    Width: Integer;
    Height: Integer;
  end;

var
  Screen, Panel, View, Status, Log, Info: TEntSize;

type
  TTerminal = class(TObject)
  private
    FChar: TEntSize;
    FWindow: TEntSize;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure Clear;
    procedure Refresh;
    procedure BackgroundColor(Value: Cardinal);
    procedure ForegroundColor(Value: Cardinal);
    procedure Print(AX, AY: Integer; AText: string; Align: Byte = 0); overload;
    procedure Print(AX, AY: Integer; AChar: Char;
      AForegroundColor: Cardinal;
      ABackgroundColor: Cardinal = 0); overload;
    procedure Print(ALeft, ATop, AWidth, AHeight: Integer; AText: string;
      Align: Byte); overload;
    function Pick(X, Y: Byte): Byte;
    property Char: TEntSize read FChar write FChar;
    property Window: TEntSize read FWindow write FWindow;
    function GetColorFromIni(AKey: string): string; overload;
    function GetColorFromIni(AKey: string; ADefault: string): Cardinal;
      overload;
    function Colorize(const AStr, AColor: string): string; overload;
    function Colorize(const ANum: Integer; const AColor: string): string; overload;
    function Icon(const ANum: string; const AColor: string = ''): string; overload;
    function GetTextScreenshot: string;
    function SetEntSize(ALeft, ATop, AWidth, AHeight: Byte): TEntSize;
  end;

var
  Terminal: TTerminal = nil;

implementation

uses SysUtils, Classes, Math, Dialogs, uGame, GNUGetText;

{ TTerminal }

procedure TTerminal.BackgroundColor(Value: Cardinal);
begin
  terminal_bkcolor(Value);
end;

procedure TTerminal.Clear;
begin
  terminal_clear;
end;

function TTerminal.Colorize(const ANum: Integer; const AColor: string): string;
begin
  Result := Format('[color=%s]%d[/color]', [LowerCase(AColor), ANum]);
end;

function TTerminal.Colorize(const AStr, AColor: string): string;
begin
  Result := Format('[color=%s]%s[/color]', [LowerCase(AColor), AStr]);
end;

constructor TTerminal.Create;
begin
  terminal_open;
  terminal_refresh;
  Self.Init;
end;

destructor TTerminal.Destroy;
begin
  terminal_close;
  inherited;
end;

procedure TTerminal.ForegroundColor(Value: Cardinal);
begin
  terminal_color(Value);
end;

function TTerminal.GetColorFromIni(AKey: string; ADefault: string): Cardinal;
begin
  Result := color_from_name(GetColorFromIni(AKey));
end;

function TTerminal.GetTextScreenshot: string;
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

function TTerminal.GetColorFromIni(AKey: string): string;
begin
  Result := LowerCase(terminal_get('ini.colors.' + LowerCase(AKey)));
end;

function TTerminal.Icon(const ANum: string; const AColor: string = ''): string;
begin
  if (AColor = '') then
  Result := Format('[font=icon][U+%s][/font]',
    [UpperCase(ANum)])
      else Result := Format('[font=icon][color=%s][U+%s][/color][/font]',
    [LowerCase(AColor), UpperCase(ANum)]);
end;

procedure TTerminal.Init;
var
  Value: TEntSize;
  Wizard: string;
begin
  Value.Width := EnsureRange(StrToIntDef(terminal_get('ini.screen.width'), 100),
    100, High(Byte));
  Value.Height := EnsureRange(StrToIntDef(terminal_get('ini.screen.height'),
    30), 30, High(Byte) div 2);
  Screen := SetEntSize(0, 0, Value.Width, Value.Height);
  Value.Width := EnsureRange(StrToIntDef(terminal_get('ini.panel.width'),
    35), 35, 50);
  Panel := SetEntSize(0, 0, Value.Width, 4);
  View := SetEntSize(1, 1, Screen.Width - Panel.Width - 3, Screen.Height - 2);
  Status := SetEntSize(View.Width + 2, 1, Panel.Width, Panel.Height);
  Log := SetEntSize(View.Width + 2, Status.Height + 4, Panel.Width,
    Screen.Height - Panel.Height - 9);
  Info := SetEntSize(View.Width + 2, Screen.Height - 4, Panel.Width, 3);
  //
  FWindow.Width := Screen.Width;
  FWindow.Height := Screen.Height;
  Wizard := '';
  if Game.Wizard then
    Wizard := '[WIZARD]';
  terminal_set(Format('window: size=%dx%d, title=%s',
    [Screen.Width, Screen.Height, Format(Trim('%s %s'), [Game.GetTitle,
    Wizard])]));
  FChar.Width := terminal_state(TK_CELL_WIDTH);
  FChar.Height := terminal_state(TK_CELL_HEIGHT);
  terminal_set(Format('icon font: Fontello.ttf, size=%dx%d, codepage=437;',
    [FChar.Width, FChar.Height]));
end;

procedure TTerminal.Print(AX, AY: Integer; AText: string; Align: Byte);
begin
  terminal_print(AX, AY, Align, AText);
end;

function TTerminal.Pick(X, Y: Byte): Byte;
begin
  Result := terminal_pick(X, Y, 0);
end;

procedure TTerminal.Print(ALeft, ATop, AWidth, AHeight: Integer; AText: string;
  Align: Byte);
begin
  terminal_print(ALeft, ATop, AWidth, AHeight, Align, AText);
end;

procedure TTerminal.Print(AX, AY: Integer; AChar: Char;
  AForegroundColor: Cardinal;
  ABackgroundColor: Cardinal = 0);
begin
  terminal_bkcolor(ABackgroundColor);
  terminal_color(AForegroundColor);
  terminal_print(AX, AY, TK_ALIGN_DEFAULT, AChar);
end;

procedure TTerminal.Refresh;
begin
  terminal_refresh;
end;

function TTerminal.SetEntSize(ALeft, ATop, AWidth, AHeight: Byte): TEntSize;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

initialization

Terminal := TTerminal.Create;

finalization

FreeAndNil(Terminal);

end.
