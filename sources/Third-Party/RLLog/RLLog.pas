unit RLLog;

interface

uses
  System.Classes, System.SysUtils;

type
  TRLLog = class(TObject)
  private
    FMsg: string;
    FLog: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Append(const S: string);
    property Msg: string read FMsg write FMsg;
    function Get(const I: Integer): string;
    function GetLast(const I: Integer): string;
    function Count: Integer;
    procedure Clear;
    procedure Turn;
  end;

implementation

{ TRLLog }

procedure TRLLog.Append(const S: string);
begin
  FMsg := FMsg + ' ' + S.Trim;
end;

procedure TRLLog.Clear;
begin
  FMsg := '';
  FLog.Clear;
end;

function TRLLog.Count: Integer;
begin
  Result := FLog.Count;
end;

constructor TRLLog.Create;
begin
  FLog := TStringList.Create;
end;

destructor TRLLog.Destroy;
begin
  FreeAndNil(FLog);
  inherited;
end;

function TRLLog.Get(const I: Integer): string;
begin
  Result := FLog[I];
end;

function TRLLog.GetLast(const I: Integer): string;
begin
  Result := FLog[Count - I];
end;

procedure TRLLog.Turn;
begin
  if not FMsg.Trim.IsEmpty then
    FLog.Append(FMsg.Trim);
end;

end.
