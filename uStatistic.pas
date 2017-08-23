unit uStatistic;

interface

type
  TStatistics = class(TObject)
  private
    FKills: Word;
    FSpCast: Word;
    FFound: Word;
    FPotDrunk: Word;
    FScrRead: Word;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Kills: Word read FKills write FKills;
    property Found: Word read FFound write FFound;
    property PotDrunk: Word read FPotDrunk write FPotDrunk;
    property ScrRead: Word read FScrRead write FScrRead;
    property SpCast: Word read FSpCast write FSpCast;
  end;

implementation

{ TStatistics }

procedure TStatistics.Clear;
begin
  Kills := 0;
  Found := 0;
  PotDrunk := 0;
  ScrRead := 0;
  SpCast := 0;
end;

constructor TStatistics.Create;
begin
  Self.Clear;
end;

destructor TStatistics.Destroy;
begin

  inherited;
end;

end.
