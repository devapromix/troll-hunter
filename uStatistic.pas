unit uStatistic;

interface

type
  TStatEnum = (stScore, stTurn, stKills, stSpCast, stFound, stPotDrunk,
    stScrRead, stItUsed, stItIdent, stItCrafted, stItRep, stFdEat);

type
  TStatistics = class(TObject)
  private
    FStat: array [TStatEnum] of Word;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Inc(const I: TStatEnum; const Value: Word = 1);
    function Get(const I: TStatEnum): Word;
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

function TStatistics.Get(const I: TStatEnum): Word;
begin
  Result := FStat[I];
end;

procedure TStatistics.Inc(const I: TStatEnum; const Value: Word = 1);
begin
  FStat[I] := FStat[I] + Value;
end;

end.
