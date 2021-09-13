unit Trollhunter.UI.Log;

interface

uses
  Trollhunter.Types,
  System.Classes,
  RLLog;

type
  TMsgLog = class(TRLLog)
  private
    FAct: string;
    function GetColor(I: Int): string;
  public
    procedure Clear;
    procedure Render(const Y: UInt = 0; Flag: Boolean = False);
    procedure Add(const S: string);
    function GetLastMsg(const ACount: Int): string;
    procedure RenderAllMessages;
  end;

var
  MsgLog: TMsgLog = nil;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.Terminal,
  BearLibTerminal,
  Trollhunter.Player,
  Trollhunter.Game;

{ TMsgLog }

const
  MaxLogCapacity = 50;

procedure TMsgLog.Add(const S: string);
begin
  Append(S);
  Game.Timer := UIntMax;
end;

procedure TMsgLog.Clear;
begin
  FAct := '';
  inherited Clear;
end;

function TMsgLog.GetLastMsg(const ACount: Int): string;
var
  I, C: Int;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    C := Math.Min(ACount, Count);
    for I := C downto 1 do
      SL.Append(Terminal.Colorize(GetLast(I), GetColor(I)));
    Result := SL.Text;
  finally
    FreeAndNil(SL);
  end;
end;

procedure TMsgLog.Render(const Y: UInt = 0; Flag: Boolean = False);
begin
  if (Flag and (Game.Timer = 0)) then
    Exit;
  Player.RenderInfo;
  if Msg.Trim.IsEmpty then
    FAct := ''
  else
    FAct := Terminal.Colorize(Msg.Trim, Terminal.GetColorFromIni('Log'));
  Terminal.ForegroundColor(clGray);
  Terminal.Print(Log.Left, Log.Top + Y, Log.Width, Log.Height, Trim(Self.GetLastMsg(MaxLogCapacity) + FAct), TK_ALIGN_BOTTOM);
end;

procedure TMsgLog.RenderAllMessages;
var
  Log, Color: string;
  I, C, LogHeight: Int;
begin
  Log := '';
  LogHeight := Terminal.Screen.Height - 3;
  for I := Count - 1 downto 0 do
    Log := Log + Terminal.Colorize(Get(I), GetColor(I)) + #13#10;
  Terminal.ForegroundColor(clGray);
  Terminal.Print(1, 2, Terminal.Screen.Width - 1, Terminal.Screen.Height - 4, Log.Trim, TK_ALIGN_TOP);
end;

function TMsgLog.GetColor(I: Int): string;
begin
  if (Odd(I)) then
    Result := 'darker  gray'
  else
    Result := 'light gray';
end;

initialization

MsgLog := TMsgLog.Create;

finalization

FreeAndNil(MsgLog);

end.
