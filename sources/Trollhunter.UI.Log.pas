unit Trollhunter.UI.Log;

interface

uses
  uTypes, Classes;

type
  TMsgLog = class(TObject)
  private
    FAct: string;
    FMsg: string;
    FLog: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(const Y: UInt = 0; Flag: Boolean = False);
    procedure Clear;
    procedure Add(S: string);
    procedure Turn;
    property Msg: string read FMsg write FMsg;
    function GetLastMsg(const ACount: Int): string;
    procedure RenderAllMessages;
  end;

var
  MsgLog: TMsgLog = nil;

implementation

uses SysUtils, Math, uTerminal, BearLibTerminal, uPlayer, uGame;

{ TMsgLog }

const
  MaxLogCapacity = 50;

procedure TMsgLog.Add(S: string);
begin
  FMsg := FMsg + ' ' + Trim(S);
  Game.Timer := UIntMax;
end;

procedure TMsgLog.Clear;
begin
  FAct := '';
  FMsg := '';
  FLog.Clear;
end;

constructor TMsgLog.Create;
begin
  FLog := TStringList.Create;
end;

destructor TMsgLog.Destroy;
begin
  FreeAndNil(FLog);
  inherited;
end;

function TMsgLog.GetLastMsg(const ACount: Int): string;
var
  I, C: Int;
  SL: TStringList;
  Color: string;
begin
  SL := TStringList.Create;
  try
    C := Math.Min(ACount, FLog.Count);
    for I := C downto 1 do
    begin
      if (Odd(I)) then
        Color := 'dark gray'
      else
        Color := 'light gray';
      SL.Append(Terminal.Colorize(FLog[FLog.Count - I], Color));
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
  if (Trim(MsgLog.Msg) = '') then
    FAct := ''
  else
    FAct := Terminal.Colorize(Trim(FMsg), Terminal.GetColorFromIni('Log'));
  Terminal.ForegroundColor(clGray);
  Terminal.Print(Log.Left, Log.Top + Y, Log.Width, Log.Height,
    Trim(Self.GetLastMsg(MaxLogCapacity) + FAct), TK_ALIGN_BOTTOM);
end;

procedure TMsgLog.RenderAllMessages;
var
  S, Color: string;
  I, C: Int;
begin
  S := '';
  C := Math.Min(Screen.Height - 3, FLog.Count);
  for I := C downto 1 do
  begin
    if (Odd(I)) then
      Color := 'dark gray'
    else
      Color := 'light gray';
    S := S + ' ' + Terminal.Colorize(FLog[FLog.Count - I], Color);
  end;
  Terminal.ForegroundColor(clGray);
  Terminal.Print(1, 2, Screen.Width - 1, Screen.Height - 4, Trim(S),
    TK_ALIGN_BOTTOM);
end;

procedure TMsgLog.Turn;
begin
  if (Trim(FMsg) <> '') then
    FLog.Append(Trim(FMsg));
end;

initialization

MsgLog := TMsgLog.Create;

finalization

FreeAndNil(MsgLog);

end.

