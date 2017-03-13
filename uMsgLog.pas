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
    property Msg: string read FMsg;
    function GetLastMsg(const ACount: Integer): string;
  end;

var
  MsgLog: TMsgLog = nil;

implementation

uses SysUtils, Math, uCommon, uTerminal, BearLibTerminal;

{ TMsgLog }

procedure TMsgLog.Add(S: string);
begin
  if (Trim(S) <> '') then
    FMsg := FMsg + ' ' + S;
end;

procedure TMsgLog.Clear;
begin
  FMsg := '';
  FLog.Clear;
end;

constructor TMsgLog.Create;
begin
  FLog := TStringList.Create;
  Self.Clear;
end;

destructor TMsgLog.Destroy;
begin
  FLog.Free;
  FLog := nil;
  inherited;
end;

function TMsgLog.GetLastMsg(const ACount: Integer): string;
var
  SL: TStringList;
  I, C: Integer;
begin
  SL := TStringList.Create;
  try
    C := Math.Min(ACount, FLog.Count);
    for I := C downto 1 do
      SL.Append(FLog[FLog.Count - I]);
    Result := SL.Text;
  finally
    SL.Free;
    SL := nil;
  end;
end;

procedure TMsgLog.Render;
var
  L: string;
begin
  if (Trim(MsgLog.Msg) = '') then
    L := '' else L := '[color=yellow]' + FMsg + '[/color]';
  Terminal.ForegroundColor(clGray);
  Terminal.Print(Log.Left, Log.Top,
  Log.Width, Log.Height, FLog.Text + L,
  TK_ALIGN_BOTTOM);
end;

procedure TMsgLog.Turn;
begin
  if (Trim(MsgLog.Msg) = '') then Exit;
  FLog.Append(FMsg);
  FMsg := '';
end;

initialization
  MsgLog := TMsgLog.Create;

finalization
  MsgLog.Free;
  MsgLog := nil;

end.
