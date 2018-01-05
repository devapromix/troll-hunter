unit uAttribute;

interface

uses uTypes;

const
  AttribMax = 100;

type
  TAttribEnum = (atDef, atMinDamage, atMaxDamage, atLife, atMaxLife, atMana,
    atMaxMana, atPV, atDV, atStr, atDex, atWil, atPer, atExtraGold, atVision,
    atSat, atLev, atExp, atReLife, atReMana, atLifeAfEachKill,
    atManaAfEachKill);

const
  AttrLow = atReLife;
  AttrHigh = atManaAfEachKill;

type
  TAttrib = record
    Value: UInt;
    Prm: UInt;
  end;

type
  TAttributes = class(TObject)
  private
    FAttrib: array [TAttribEnum] of TAttrib;
    function GetAttrib(I: TAttribEnum): TAttrib;
    procedure SetAttrib(I: TAttribEnum; const Value: TAttrib);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Attrib[I: TAttribEnum]: TAttrib read GetAttrib write SetAttrib;
    procedure Modify(I: TAttribEnum; AValue: Int; APrm: Int = 0);
    procedure SetValue(I: TAttribEnum; AValue: Int);
  end;

implementation

uses Math, uHelpers;

{ TAttributes }

procedure TAttributes.Clear;
var
  I: TAttribEnum;
begin
  for I := Low(FAttrib) to High(FAttrib) do
  begin
    FAttrib[I].Value := 0;
    FAttrib[I].Prm := 0;
  end;
end;

constructor TAttributes.Create;
begin

end;

destructor TAttributes.Destroy;
begin

  inherited;
end;

function TAttributes.GetAttrib(I: TAttribEnum): TAttrib;
begin
  Result := FAttrib[I];
end;

procedure TAttributes.Modify(I: TAttribEnum; AValue, APrm: Int);
begin
  FAttrib[I].Value := FAttrib[I].Value + AValue;
  FAttrib[I].Prm := FAttrib[I].Prm + APrm;
end;

{procedure TAttributes.ModifyValue(I: TAttribEnum; AValue: Int);
begin
  Modify(I, AValue);
  case I of
    atLife:
      FAttrib[I].Value := FAttrib[I].Value.InRange(FAttrib[atMaxLife].Value);
    atMana:
      FAttrib[I].Value := FAttrib[I].Value.InRange(FAttrib[atMaxMana].Value);
  end;
end;}

procedure TAttributes.SetAttrib(I: TAttribEnum; const Value: TAttrib);
begin
  FAttrib[I] := Value;
end;

procedure TAttributes.SetValue(I: TAttribEnum; AValue: Int);
begin
  FAttrib[I].Value := AValue;
end;

end.
