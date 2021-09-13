unit uBar;

interface

type
  TBar = class(TObject)
  private
    FCur: Integer;
    FMax: Integer;
    function GetCur: Integer;
    function GetMax: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function IsMin: Boolean;   
    function IsMax: Boolean;
    procedure SetCur(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure Dec(Value: Integer = 1);
    procedure Inc(Value: Integer = 1);
    property Cur: Integer read GetCur write SetCur;
    property Max: Integer read GetMax write SetMax;
    procedure SetToMin;
    procedure SetToMax;
  end;


implementation

{ TBar }

constructor TBar.Create;
begin
         
end;

procedure TBar.Dec(Value: Integer);
begin
  if ((Cur > 0) and (Value > 0)) then SetCur(GetCur - Value);
  if (Cur < 0) then Cur := 0;
end;

destructor TBar.Destroy;
begin

  inherited;
end;

function TBar.GetCur: Integer;
begin
  Result := FCur;
end;

function TBar.GetMax: Integer;
begin
  Result := FMax;
end;

procedure TBar.Inc(Value: Integer);
begin
  if ((Cur < Max) and (Value > 0)) then SetCur(GetCur + Value);
  if (Cur > Max) then Cur := Max;
end;

function TBar.IsMax: Boolean;
begin
  Result := (Cur = Max);
end;

function TBar.IsMin: Boolean;
begin
  Result := (Cur = 0);
end;

procedure TBar.SetCur(Value: Integer);
begin
  if (Value < 0) then Value := 0;
  if (Value > Max) then Value := Max;
  FCur := Value;
end;

procedure TBar.SetMax(Value: Integer);
begin
  if (Value < 0) then Value := 0;
  FMax := Value;
end;

procedure TBar.SetToMax;
begin
  Cur := Max;
end;

procedure TBar.SetToMin;
begin
  Cur := 0;
end;

end.
