unit Trollhunter.Helpers;

interface

uses
  Math,
  Classes,
  Trollhunter.Types,
  Trollhunter.Mob.Types,
  Trollhunter.Item.Affixes,
  Trollhunter.Item.Types;

type
  TIntHelper = record helper for Int
  public
    function Percent(AValue: Int): Int;
    function InRange(AMaxValue: Int): Int;
    function ToString: string;
    function ItemEnum: TItemEnum;
    function SuffixEnum: TSuffixEnum;
  end;

type
  TUIntHelper = record helper for UInt
  public
    function Percent(AValue: Int): Int; inline;
    function InRange(AMaxValue: UInt): UInt; inline;
    function ToString: string;
    function MobEnum: TMobEnum;
    function ItemEnum: TItemEnum;
  end;

type
  TStringHelper = record helper for
    string
    function GetName(const Pref: string): string;
  end;

implementation

uses
  SysUtils;

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

function TIntHelper.ItemEnum: TItemEnum;
begin
  Result := TItemEnum(Self);
end;

function TIntHelper.SuffixEnum: TSuffixEnum;
begin
  Result := TSuffixEnum(Self);
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

function TUIntHelper.MobEnum: TMobEnum;
begin
  Result := TMobEnum(Self);
end;

function TUIntHelper.ItemEnum: TItemEnum;
begin
  Result := TItemEnum(Self);
end;

{ TStringHelper }

function TStringHelper.GetName(const Pref: string): string;
begin
  Result := StringReplace(Self, Pref, '', [rfReplaceAll]);
  Result := StringReplace(Result, '_', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '1', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '2', '(', [rfReplaceAll]);
  Result := StringReplace(Result, '3', ')', [rfReplaceAll]);
  Result := StringReplace(Result, '4', '-', [rfReplaceAll]);
end;

end.
