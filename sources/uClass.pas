unit uClass;

interface

type
  TClassEnum = (clWarrior, clMage);

type
  TClasses = class(TObject)
  private
    FClassName: array [TClassEnum] of string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetName(I: TClassEnum): string;
  end;

var
  Classes: TClasses;

implementation

uses SysUtils, TypInfo, uHelpers;

{ TClasses }

constructor TClasses.Create;
var
  I: TClassEnum;
  P: Pointer;
begin
  P := TypeInfo(TClassEnum);
  for I := Low(TClassEnum) to High(TClassEnum) do
    FClassName[I] := GetEnumName(P, Ord(I)).GetName('cl');
end;

destructor TClasses.Destroy;
begin

  inherited;
end;

function TClasses.GetName(I: TClassEnum): string;
begin
  Result := FClassName[I]
end;

initialization

Classes := TClasses.Create;

finalization

FreeAndNil(Classes);

end.
