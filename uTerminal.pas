unit uTerminal;

interface

uses BearLibTerminal, uCommon;

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
    procedure Print(AX, AY: Integer; AText: string; Align: Byte = 0);
    property Char: TEntSize read FChar write FChar;
    property Window: TEntSize read FWindow write FWindow;
  end;

var
  Terminal: TTerminal = nil;

implementation

uses SysUtils;

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
begin
  Value.Width := Clamp(StrToIntDef(terminal_get('ini.screen.width'), 80), 80, 255);
  Value.Height := Clamp(StrToIntDef(terminal_get('ini.screen.height'), 25), 25, 128);
  Screen := SetEntSize(0, 0, Value.Width, Value.Height);
  Value.Width := Clamp(StrToIntDef(terminal_get('ini.panel.width'), 30), 30, 50);
  Panel := SetEntSize(0, 0, Value.Width, 3);
  View := SetEntSize(1, 1, Screen.Width - Panel.Width - 3, Screen.Height - 2);
  Status := SetEntSize(View.Width + 2, 1, Panel.Width, Panel.Height);
  Log := SetEntSize(View.Width + 2, Status.Height + 2, Panel.Width, Screen.Height - Panel.Height - 3);
  //
  FWindow.Width := Screen.Width;
  FWindow.Height := Screen.Height;
  terminal_set(Format('window: size=%dx%d, title=%s v.%s', [Screen.Width, Screen.Height,
    'Trollhunter', Version]));
  FChar.Width := terminal_state(TK_CELL_WIDTH);
  FChar.Height := terminal_state(TK_CELL_HEIGHT);
end;

procedure TTerminal.Print(AX, AY: Integer; AText: string; Align: Byte);
begin
  terminal_print(AX, AY, Align, AText);
end;

procedure TTerminal.Refresh;
begin
  terminal_refresh;
end;

initialization
  Randomize;
  Terminal := TTerminal.Create;
  Terminal.Init;

finalization
  Terminal.Free;
  Terminal := nil;

end.
