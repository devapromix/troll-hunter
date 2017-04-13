unit uMsgLog;

interface

uses
  Classes;

type
  TMsgLog = class(TObject)
  private
    FMsg: string;
    FLog: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render;
    procedure Clear;
    procedure Add(S: string);
    procedure Turn;
    property Msg: string read FMsg write FMsg;
    function GetLastMsg(const ACount: Integer): string;
  end;

var
  MsgLog: TMsgLog = nil;

implementation

uses SysUtils, Math, uCommon, uTerminal, BearLibTerminal;

{ TMsgLog }

const
  MaxLogCapacity = 25;

procedure TMsgLog.Add(S: string);
begin
  FMsg := FMsg + ' ' + Trim(S);
end;

procedure TMsgLog.Clear;
begin
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

function TMsgLog.GetLastMsg(const ACount: Integer): string;
var
  I, C: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    C := Math.Min(ACount, FLog.Count);
    for I := C downto 1 do
      SL.Append(FLog[FLog.Count - I]);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TMsgLog.Render;
var
  L: string;
begin
  if (Trim(MsgLog.Msg) = '') then L := '' else
    L := Format('[color=%s]%s[/color]',
    [LowerCase(terminal_get('ini.colors.log')), Trim(FMsg)]);
  Terminal.ForegroundColor(clGray);
  Terminal.Print(Log.Left, Log.Top, Log.Width, Log.Height,
    Trim(Self.GetLastMsg(MaxLogCapacity) + L),
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
