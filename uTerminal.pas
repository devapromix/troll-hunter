unit uTerminal;

interface

uses Types, BearLibTerminal, uCommon;

type
  TSize = Types.TSize;

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
      AForegroundColor: Cardinal; ABackgroundColor: Cardinal = 0); overload;
    procedure Print(ALeft, ATop, AWidth, AHeight: Integer; AText: string;
      Align: Byte); overload;
    function Pick(X, Y: Byte): Byte;
    property Char: TEntSize read FChar write FChar;
    property Window: TEntSize read FWindow write FWindow;
  end;

var
  Terminal: TTerminal = nil;

implementation

uses SysUtils, Dialogs, uGame;

{ TTerminal }

procedure TTerminal.BackgroundColor(Value: Cardinal);
begin
  terminal_bkcolor(Value);
end;

procedure TTerminal.Clear;
begin
  terminal_clear;
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

procedure TTerminal.Init;
var
  Value: TEntSize;
  Wizard: string;
begin
  Value.Width := Clamp(StrToIntDef(terminal_get('ini.screen.width'),
    80), 80, High(Byte));
  Value.Height := Clamp(StrToIntDef(terminal_get('ini.screen.height'),
    30), 30, High(Byte) div 2); 
  Screen := SetEntSize(0, 0, Value.Width, Value.Height);
  Value.Width := Clamp(StrToIntDef(terminal_get('ini.panel.width'),
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
    [Screen.Width, Screen.Height, Trim('Trollhunter ' + Wizard)]));
  FChar.Width := terminal_state(TK_CELL_WIDTH);
  FChar.Height := terminal_state(TK_CELL_HEIGHT);
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
  AForegroundColor: Cardinal; ABackgroundColor: Cardinal = 0);
begin
  terminal_bkcolor(ABackgroundColor);
  terminal_color(AForegroundColor);
  terminal_print(AX, AY, TK_ALIGN_DEFAULT, AChar);
end;

procedure TTerminal.Refresh;
begin
  terminal_refresh;
end;

initialization
  Terminal := TTerminal.Create;

finalization
  FreeAndNil(Terminal);

end.
