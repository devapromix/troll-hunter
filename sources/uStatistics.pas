unit uStatistics;

interface

uses Classes;

type
  TStatisticsEnum = (stTilesMoved, stKills, stSpCast, stFound, stPotDrunk,
    stScrRead, stItUsed, stItIdent, stItCrafted, stItRep, stFdEat,
    stCoinsLooted);

type
  TStatistics = class(TObject)
  private
    FF: TStringList;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Inc(const I: TStatisticsEnum; const Value: Cardinal = 1);
    function Get(const I: TStatisticsEnum): Cardinal;
    procedure SetValue(const I: TStatisticsEnum; const Value: Cardinal);
    property Text: string read GetText write SetText;
  end;

implementation

uses SysUtils;

{ TStatistics }

procedure TStatistics.Clear;
var
  I: TStatisticsEnum;
begin
  FF.Clear;
  for I := Low(TStatisticsEnum) to High(TStatisticsEnum) do
    FF.Append('0');
end;

constructor TStatistics.Create;
begin
  FF := TStringList.Create;
  Self.Clear;
end;

destructor TStatistics.Destroy;
begin
  FF.Free;
  inherited;
end;

function TStatistics.Get(const I: TStatisticsEnum): Cardinal;
begin
  Result := StrToIntDef(FF[Ord(I)], 0);
end;

function TStatistics.GetText: string;
begin
  Result := FF.Text;
end;

procedure TStatistics.Inc(const I: TStatisticsEnum; const Value: Cardinal);
var
  N: Cardinal;
begin
  N := StrToIntDef(FF[Ord(I)], 0);
  N := N + Value;
  FF[Ord(I)] := IntToStr(N);
end;

procedure TStatistics.SetText(const Value: string);
begin
  FF.Text := Value;
end;

procedure TStatistics.SetValue(const I: TStatisticsEnum; const Value: Cardinal);
begin
  FF[Ord(I)] := IntToStr(Value);
end;

end.
