unit uHelpers;

interface

uses uTypes, uBearLibItemsCommon, uPlayer;

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
  TPlayerHelper = class helper for TPlayer
    function ToString2: string;
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

{ TPlayerHelper }

function TPlayerHelper.ToString2: string;
begin

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

end.
