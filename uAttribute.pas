unit uAttribute;

interface

const
  AtrMax = 100;

type
  TAtrEnum = (atDef, atMinDamage, atMaxDamage, atLife, atMaxLife, atMana, atMaxMana,
    atPV, atDV, atStr, atDex, atWil, atPer, atVis, atSat, atLev, atExp);

type
  TAtr = record
    Value: Word;
    Prm: Word;
  end;

type
  TAttributes = class(TObject)
  private
    FAtr: array [TAtrEnum] of TAtr;
    function GetAtr(I: TAtrEnum): TAtr;
    procedure SetAtr(I: TAtrEnum; const Value: TAtr);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Atr[I: TAtrEnum]: TAtr read GetAtr write SetAtr;
    procedure Modify(I: TAtrEnum; AValue: Integer; APrm: Integer = 0);
    procedure SetValue(I: TAtrEnum; AValue: Integer);
  end;

implementation

{ TAttributes }

procedure TAttributes.Clear;
var
  I: TAtrEnum;
begin
  for I := Low(FAtr) to High(FAtr) do
  begin
    FAtr[I].Value := 0;
    FAtr[I].Prm := 0;
  end;
end;

constructor TAttributes.Create;
begin

end;

destructor TAttributes.Destroy;
begin

  inherited;
end;

function TAttributes.GetAtr(I: TAtrEnum): TAtr;
begin
  Result := FAtr[I];
end;

procedure TAttributes.Modify(I: TAtrEnum; AValue, APrm: Integer);
begin
  FAtr[I].Value := FAtr[I].Value + AValue;
  FAtr[I].Prm := FAtr[I].Prm + APrm;
end;

procedure TAttributes.SetAtr(I: TAtrEnum; const Value: TAtr);
begin
  FAtr[I] := Value;
end;

procedure TAttributes.SetValue(I: TAtrEnum; AValue: Integer);
begin
  FAtr[I].Value := AValue;
end;

end.
