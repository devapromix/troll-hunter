unit uMsgLog;

interface

uses
  Classes;

type
  TMsgLog = class(TObject)
  private
    FLog: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render;
  end;

var
  MsgLog: TMsgLog = nil;

implementation

uses uCommon, uTerminal, BearLibTerminal;

{ TMsgLog }

constructor TMsgLog.Create;
begin
  FLog := TStringList.Create;
end;

destructor TMsgLog.Destroy;
begin
  FLog.Free;
  FLog := nil;
  inherited;
end;

procedure TMsgLog.Render;
begin
  Terminal.Print(Log.Left, Log.Top, Log.Width, Log.Height,
  'sdfasd sdfasd sdfasdf sdafasd sdfsad sadfasdf sadfasdf asdfasd asdfasdf',
  TK_ALIGN_BOTTOM);
end;

initialization
  MsgLog := TMsgLog.Create;

finalization
  MsgLog.Free;
  MsgLog := nil;

end.
