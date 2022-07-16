unit Trollhunter.Error;

interface

uses SysUtils, Classes;

type
  TError = class(TObject)
  private
    FLog: TStringList;
  public
    procedure Add(S: string); overload;
    procedure Add(S, V: string); overload;
    constructor Create;
    destructor Destroy; override;
  end;

var
  Error: TError;

implementation

{ TError }

procedure TError.Add(S: string);
begin
  FLog.Append(Trim(S));
end;

procedure TError.Add(S, V: string);
begin
  Add(S + ': ' + V);
end;

constructor TError.Create;
begin
  FLog := TStringList.Create;
end;

destructor TError.Destroy;
begin
  with FLog do
    if (FLog.Count > 0) then
      SaveToFile('errors.txt');
  FLog.Free;
  inherited;
end;

initialization

Error := TError.Create;

finalization

Error.Free;

end.
