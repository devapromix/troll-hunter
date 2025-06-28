unit Trollhunter.UI.Log;

interface

uses
  Trollhunter.Types,
  Classes;

type
  TMsgLog = class(TObject)
  private
    FAct: string;
    FMsg: string;
    FLog: TStringList;
    function GetColorByIndex(const AIndex: Int): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(const Y: UInt = 0; Flag: Boolean = False);
    procedure Clear;
    procedure Add(const AMsg: string);
    procedure Turn;
    property Msg: string read FMsg write FMsg;
    function GetLastMsg(const ACount: Int): string;
    procedure RenderAllMessages;
  end;

var
  MsgLog: TMsgLog = nil;

implementation

uses
  SysUtils,
  StrUtils,
  Math,
  Trollhunter.Terminal,
  BearLibTerminal,
  Trollhunter.Player,
  uGame;

{ TMsgLog }

const
  MAX_LOG_CAPACITY = 50;

function TMsgLog.GetColorByIndex(const AIndex: Int): string;
begin
  Result := IfThen(Odd(AIndex), 'dark gray', 'light gray');
end;

procedure TMsgLog.Add(const AMsg: string);
begin
  FMsg := FMsg + ' ' + Trim(AMsg);
  Game.Timer := UIntMax;
  while FLog.Count >= MAX_LOG_CAPACITY do
    FLog.Delete(0);
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
  LStringList: TStringList;
begin
  LStringList := TStringList.Create;
  try
    C := Math.Min(ACount, FLog.Count);
    for I := C downto 1 do
      LStringList.Append(Terminal.Colorize(FLog[FLog.Count - I],
        GetColorByIndex(I)));
    Result := LStringList.Text;
  finally
    FreeAndNil(LStringList);
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
    Trim(Self.GetLastMsg(MAX_LOG_CAPACITY) + FAct), TK_ALIGN_BOTTOM);
end;

procedure TMsgLog.RenderAllMessages;
var
  S: string;
  I, C: Int;
begin
  S := '';
  C := Math.Min(Screen.Height - 3, FLog.Count);
  for I := C downto 1 do
    S := S + ' ' + Terminal.Colorize(FLog[FLog.Count - I], GetColorByIndex(I));
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
