unit uTypes;

interface

type
  Int = System.Integer;
  UInt = System.Word;

type
  IntHelper = record helper for Int
    function ToString(): string;
  end;

type
  UIntHelper = record helper for UInt
    function ToString(): string;
  end;

const
  UIntMax = High(Byte);

implementation

uses SysUtils;

{ IntHelper }

function IntHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ UIntHelper }

function UIntHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

end.
