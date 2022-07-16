unit Trollhunter.Log;

interface

uses
  SysUtils,
  Classes;

type
  TLog = class
  private
    FCount: Integer;
    FLast: string;
    FLine: string;
    FF: TStringList;
    procedure AddToLog(Msg: string);
    function GetText: string;
    procedure SetText(const Value: string);
  public
    property Text: string read GetText write SetText;
    procedure Add(S: string);
    procedure Clear;
    procedure Apply;
    procedure Render;
    constructor Create;
    destructor Destroy; override;
  end;

var
  Log: TLog;
  LogMax: Integer = 12;

implementation

uses
  Trollhunter.Utils,
  Trollhunter.Graph,
  Trollhunter.Color,
  Trollhunter.AStar;

{ TLog }

procedure TLog.Add(S: string);
begin
  S := Trim(S);
  if (S = '') then
    Exit;
  FLine := FLine + S + #32;
end;

procedure TLog.AddToLog(Msg: string);
var
  E: TExplodeResult;
  F: TExplodeResult;
begin
  E := nil;
  F := nil;
  Msg := Trim(Graph.Text.ClearText(Msg));
  if (FF.Count > 0) then
  begin
    FLast := FF[FF.Count - 1];
    E := Explode('[', FLast);
    FLast := Trim(E[0]);
    if (High(E) > 1) then
    begin
      F := Explode(']', E[1]);
      FCount := StrToInt(F[0]);
    end;
  end
  else
    FLast := '';
  if (Msg <> '') then
  begin
    if (Msg = FLast) then
    begin
      if (FLast <> '') then
      begin
        FCount := FCount + 1;
        FF[FF.Count - 1] := Format('%s [%d]', [FLast, FCount]);
      end;
    end
    else
    begin
      FCount := 1;
      FF.Append(Msg);
    end;
  end;
  while (FF.Count > LogMax) do
    FF.Delete(0);
end;

constructor TLog.Create;
begin
  FF := TStringList.Create;
  FLast := '';
  FLine := '';
  FCount := 1;
end;

destructor TLog.Destroy;
begin
  FF.Free;
  inherited;
end;

procedure TLog.Apply;
begin
  AddToLog(FLine);
  FLine := '';
end;

procedure TLog.Render;
var
  P, Top: Integer;
  I, J, C, Y, W: Integer;
  K: TExplodeResult;
  A: TStringList;
begin
  K := nil;
  with Graph.Surface.Canvas do
  begin
    Top := (((Graph.CharHeight * 6) + (MapSide * 2)) div Graph.CharHeight) + 1;
    LogMax := (Graph.Height div Graph.CharHeight) - Top;
    W := Graph.PW div Graph.CharWidth;
    if (Log.FF.Count = 0) then
      Exit;
    P := 0;
    Y := Top;
    A := TStringList.Create;
    for I := 0 to Log.FF.Count - 1 do
    begin
      K := Explode(W, Log.FF[I]);
      for J := 0 to High(K) do
        A.Append(K[J]);
      P := Length(K);
    end;
    J := A.Count - LogMax;
    if (J < 0) then
      J := 0;
    for I := J to A.Count - 1 do
    begin
      if (I > A.Count - P - 1) then
        C := cRdYellow
      else
        C := cRdPurple;
      Font.Color := C;
      TextOut(Graph.DL + 4, (Y * Graph.CharHeight), A[I]);
      Inc(Y);
    end;
    A.Free;
    // TextOut(Graph.DL + 4, Top * Graph.CharHeight, 'D');
  end;
end;

procedure TLog.Clear;
begin
  FLine := '';
  FF.Clear;
end;

function TLog.GetText: string;
begin
  Result := FF.Text;
end;

procedure TLog.SetText(const Value: string);
begin
  Clear;
  FF.Text := Value;
end;

initialization

Log := TLog.Create;

finalization

Log.Free;

end.
