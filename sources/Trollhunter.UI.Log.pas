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
  Color: string;
begin
  SL := TStringList.Create;
  try
    C := Math.Min(ACount, Count);
    for I := C downto 1 do
    begin
      if (Odd(I)) then
        Color := 'dark gray'
      else
        Color := 'light gray';
      SL.Append(Terminal.Colorize(Get(I), Color));
    end;
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
  S, Color: string;
  I, C: Int;
begin
  S := '';
  C := Math.Min(Terminal.Screen.Height - 3, Count);
  for I := C downto 1 do
  begin
    if (Odd(I)) then
      Color := 'dark gray'
    else
      Color := 'light gray';
    S := S + ' ' + Terminal.Colorize(Get(I), Color);
  end;
  Terminal.ForegroundColor(clGray);
  Terminal.Print(1, 2, Terminal.Screen.Width - 1, Terminal.Screen.Height - 4, S.Trim, TK_ALIGN_TOP);
end;

initialization

MsgLog := TMsgLog.Create;

finalization

FreeAndNil(MsgLog);

end.
