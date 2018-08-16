unit Trollhunter.Statistic;

interface

uses Trollhunter.Types;

type
  TStatEnum = (stScore, stTurn, stKills, stSpCast, stFound, stPotDrunk, stScrRead, stItUsed, stItIdent, stItCrafted, stItRep, stFdEat, stAge,
    stWeight, stHeight, stMetabolism, stCoinsLooted);

type
  TStatistics = class(TObject)
  private
    FStat: array [TStatEnum] of UInt;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Inc(const I: TStatEnum; const Value: UInt = 1);
    function Get(const I: TStatEnum): UInt;
    procedure SetValue(const I: TStatEnum; const Value: UInt);
  end;

implementation

{ TStatistics }

procedure TStatistics.Clear;
var
  I: TStatEnum;
begin
  for I := Low(TStatEnum) to High(TStatEnum) do
    FStat[I] := 0;
end;

constructor TStatistics.Create;
begin
  Self.Clear;
end;

destructor TStatistics.Destroy;
begin

  inherited;
end;

function TStatistics.Get(const I: TStatEnum): UInt;
begin
  Result := FStat[I];
end;

procedure TStatistics.Inc(const I: TStatEnum; const Value: UInt = 1);
begin
  FStat[I] := FStat[I] + Value;
end;

procedure TStatistics.SetValue(const I: TStatEnum; const Value: UInt);
begin
  FStat[I] := Value;
end;

end.
