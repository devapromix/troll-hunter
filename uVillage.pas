unit uVillage;

interface

type
  TVillageEnum = (veDork);

type
  TVillage = record

  end;

type
  TVillages = class(TObject)
    constructor Create;
    destructor Destroy; override;
  end;

var
  Villages: TVillages;

implementation

uses SysUtils;

//What can I get you today?
//The %s just frowns. Maybe you'll return when you have enough gold?

{ TVillages }

constructor TVillages.Create;
begin

end;

destructor TVillages.Destroy;
begin

  inherited;
end;

initialization
  Villages := TVillages.Create;

finalization
  FreeAndNil(Villages);

end.
