unit Trollhunter.Helpers;

interface

uses Classes,
  Trollhunter.Types,
  uBearLibItemsCommon,
  Trollhunter.Player;

type
  TIntHelper = record helper for Int
  public
    function Percent(AValue: Int): Int; inline;
    function InRange(AMaxValue: Int): Int; inline;
    function ToString: string;
  end;

type
  TUIntHelper = record helper for UInt
  public
    function Percent(AValue: Int): Int; inline;
    function InRange(AMaxValue: UInt): UInt; inline;
    function ToString: string;
  end;

type
  TStringHelper = record helper for
    string
    function GetName(const Pref: string): string;
  end;

type
  TStringListHelper = class helper for TStringList
    function Join(const CharSeparator: Char): string;
    function Explode(const CharSeparator: Char; const Source: string)
      : TStringList;
  end;

type
  TItemHelper = record helper for Item
    function DoIt: string;
  end;

implementation

uses SysUtils, Math;

{ IntHelper }

function TIntHelper.InRange(AMaxValue: Int): Int;
begin
  Result := EnsureRange(Self, 0, AMaxValue);
end;

function TIntHelper.Percent(AValue: Int): Int;
begin
  Result := Round(Self * AValue / 100);
end;

function TIntHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ UIntHelper }

function TUIntHelper.InRange(AMaxValue: UInt): UInt;
begin
  Result := EnsureRange(Self, 0, AMaxValue);
end;

function TUIntHelper.Percent(AValue: Int): Int;
begin
  Result := Round(Self * AValue / 100);
end;

function TUIntHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ TItemHelper }

function TItemHelper.DoIt: string;
begin

end;

{ TStringHelper }

function TStringHelper.GetName(const Pref: string): string;
begin
  Result := StringReplace(Self, Pref, '', [rfReplaceAll]);
  Result := StringReplace(Result, '_', ' ', [rfReplaceAll]);
end;

{ TStringListHelper }

function TStringListHelper.Join(const CharSeparator: Char): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Self.Count - 1 do
    if (I < Self.Count - 1) then
      Result := Result + Self[I] + CharSeparator
    else
      Result := Result + Self[I];
end;

function TStringListHelper.Explode(const CharSeparator: Char;
  const Source: string): TStringList;
var
  I: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  Strings.Delimiter := CharSeparator;
  Strings.DelimitedText := Source;
  for I := Strings.Count - 1 downto 0 do
    if (Strings[I] = '') then
      Strings.Delete(I);
  Result := Strings;
end;

end.
