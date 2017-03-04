unit uTerminal;

interface

uses BearLibTerminal, uCommon;

type
  TTerminal = class(TObject)
  private

  public
    constructor Create;
    destructor Destroy; override;
  end;

var
  Terminal: TTerminal = nil;

implementation

{ TTerminal }

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

initialization
  Terminal := TTerminal.Create;

finalization
  Terminal.Free;
  Terminal := nil;

end.
