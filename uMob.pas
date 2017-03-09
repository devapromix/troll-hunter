unit uMob;

interface

type
  TGetXYVal = function(X, Y: integer): boolean;stdcall;

implementation

function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer; Callback: TGetXYVal; var TargetX, TargetY: integer): boolean;external 'BeaRLibPF.dll';

function MyCallback(X, Y: Integer): Boolean; stdcall;
begin
  Result := MapObjects[X, Y] = teNone;
end;
end.
