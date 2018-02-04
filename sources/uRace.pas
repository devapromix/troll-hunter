unit uRace;

interface

type
  TRace = class(TObject)

  end;

var
  Race: TRace;

implementation

uses SysUtils;

initialization
  Race := TRace.Create;

finalization
  FreeAndNil(Race);

end.
