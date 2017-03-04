unit uTerminal;

interface

uses BearLibTerminal, uCommon;

type
  TTerminal = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Refresh;
    procedure BackgroundColor(Value: Cardinal);
    procedure ForegroundColor(Value: Cardinal);
    procedure Print(AX, AY: Integer; AText: string; Align: Byte = 0);
    function Clamp(Value, AMin, AMax: Integer; Flag: Boolean = True): Integer;
  end;

var
  Terminal: TTerminal = nil;

implementation

{ TTerminal }

procedure TTerminal.BackgroundColor(Value: Cardinal);
begin
  terminal_bkcolor(Value);
end;

function TTerminal.Clamp(Value, AMin, AMax: Integer;
  Flag: Boolean): Integer;
begin
  Result := Value;
  if (Result < AMin) then
    if Flag then
      Result := AMin
    else
      Result := AMax;
  if (Result > AMax) then
    if Flag then
      Result := AMax
    else
      Result := AMin;
end;

procedure TTerminal.Clear;
begin
  terminal_clear;
end;

constructor TTerminal.Create;
begin
  terminal_open;
  terminal_refresh;
end;

destructor TTerminal.Destroy;
begin
  terminal_close;
  inherited;
end;

procedure TTerminal.ForegroundColor(Value: Cardinal);
begin
  terminal_color(Value);
end;

procedure TTerminal.Print(AX, AY: Integer; AText: string; Align: Byte);
begin

end;

procedure TTerminal.Refresh;
begin
  terminal_refresh;
end;

initialization
  Randomize;
  Terminal := TTerminal.Create;

finalization
  Terminal.Free;
  Terminal := nil;

end.
