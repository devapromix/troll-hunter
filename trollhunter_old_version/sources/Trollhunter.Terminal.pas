unit Trollhunter.Terminal;

interface

uses
  Types,
  PasTerminal,
  BearLibTerminal,
  Trollhunter.Types;

type
  TGlyph = record
    Symbol: string;
    ForegroundColor: Cardinal;
    BackgroundColor: Cardinal;
  end;

type
  TSize = Types.TSize;

var
  Panel, View, Status, Log, Info: TEntSize;

type
  TTerminal = class(TPasTerminal)
  private
    FChar: TEntSize;
    FScreen: TEntSize;
    FWindow: TEntSize;
  public
    constructor Create;
    procedure Init;
    property Char: TEntSize read FChar write FChar;
    property Screen: TEntSize read FScreen write FScreen;
    property Window: TEntSize read FWindow write FWindow;
    function GetTextScreenshot: string;
  end;

var
  Terminal: TTerminal = nil;

implementation

uses
  SysUtils,
  Classes,
  Math;

{ TTerminal }

constructor TTerminal.Create;
begin
  inherited Create;
  Self.Init();
end;

procedure TTerminal.Init;
var
  Value: TEntSize;
begin
  Value.Width := EnsureRange(StrToIntDef(terminal_get('ini.screen.width'), 100), 100, UIntMax);
  Value.Height := EnsureRange(StrToIntDef(terminal_get('ini.screen.height'), 30), 30, UIntMax div 2);
  FScreen := SetEntSize(0, 0, Value.Width, Value.Height);
  Value.Width := EnsureRange(StrToIntDef(terminal_get('ini.panel.width'), 35), 35, 50);
  Panel := SetEntSize(0, 0, Value.Width, 4);
  View := SetEntSize(1, 1, FScreen.Width - Panel.Width - 3, FScreen.Height - 2);
  Status := SetEntSize(View.Width + 2, 1, Panel.Width, Panel.Height);
  Log := SetEntSize(View.Width + 2, Status.Height + 4, Panel.Width, FScreen.Height - Panel.Height - 9);
  Info := SetEntSize(View.Width + 2, FScreen.Height - 4, Panel.Width, 3);
  //
  FWindow.Width := Screen.Width;
  FWindow.Height := Screen.Height;
  terminal_set(Format('window.size=%dx%d', [Screen.Width, Screen.Height]));
  FChar.Width := terminal_state(TK_CELL_WIDTH);
  FChar.Height := terminal_state(TK_CELL_HEIGHT);
  terminal_set(Format('icon font: Fontello.ttf, size=%dx%d, codepage=437;', [Round(FChar.Width * 1.4), Round(FChar.Height * 1.4)]));
end;

function TTerminal.GetTextScreenshot: string;
var
  SL: TStringList;
  X, Y, C: UInt;
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

initialization

Terminal := TTerminal.Create;

finalization

FreeAndNil(Terminal);

end.
