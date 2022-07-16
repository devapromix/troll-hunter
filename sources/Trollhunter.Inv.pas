unit Trollhunter.Inv;

interface

uses
  Classes;

type
  TRecInv = record
    ID: string;
    Stack, Doll: Boolean;
    Count, Weight, Tough: Integer;
  end;

  TInv = class(TObject)
  private
    FItem: array [1 .. 26] of TRecInv;
    FMaxCount: Integer;
    FMaxWeight: Integer;
    function IndexOf(ID: string): Integer;
    procedure SetMaxCount(Value: Integer);
    procedure SetMaxWeight(Value: Integer);
  public
    constructor Create(AMaxCount, AMaxWeight: Integer);
    destructor Destroy; override;
    property MaxCount: Integer read FMaxCount write SetMaxCount;
    property MaxWeight: Integer read FMaxWeight write SetMaxWeight;
    procedure Clear(AMaxCount, AMaxWeight: Integer); overload;
    procedure Clear; overload;
    procedure Empty(I: Integer);
    procedure SetTough(ID, ATough: Integer);
    function Add(ID: string; ACount, AWeight, ATough: Integer;
      AStack: Boolean): Boolean;
    function Del(ID: string; ACount: Integer = 1): Boolean; overload;
    function Del(I: Integer; ACount: Integer = 1): Boolean; overload;
    function Count: Integer;
    function Weight: Integer;
    function GetID(I: Integer): string;
    function GetCount(ID: Integer): Integer; overload;
    function GetCount(ID: string): Integer; overload;
    function GetTough(I: Integer): Integer;
    function GetDoll(I: Integer): Boolean;
    function GetStack(I: Integer): Boolean;
    function GetWeight(I: Integer): Integer;
    function GetItemWeight(I: Integer): Integer;
  end;

  TAdvInv = class(TInv)
  private
    FF: TStringList;
    procedure Save;
    procedure Load;
    function GetText: string;
    procedure SetText(const Value: string);
  public
    property Text: string read GetText write SetText;
    function Equip(I: Integer): Boolean;
    function UnEquip(I: Integer): Boolean;
    constructor Create(AMaxCount, AMaxWeight: Integer); overload;
    constructor Create; overload;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Trollhunter.Utils;

constructor TInv.Create(AMaxCount, AMaxWeight: Integer);
begin
  Clear(AMaxCount, AMaxWeight);
end;

destructor TInv.Destroy;
begin

end;

function TInv.GetID(I: Integer): string;
begin
  Result := FItem[I].ID;
end;

function TInv.GetCount(ID: Integer): Integer;
begin
  Result := FItem[ID].Count;
  if (Result < 0) then
    Result := 0;
end;

function TInv.GetCount(ID: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if (ID = '') then
    Exit;
  I := IndexOf(ID);
  if (I = 0) then
    Exit;
  Result := GetCount(I);
end;

function TInv.GetWeight(I: Integer): Integer;
begin
  Result := FItem[I].Weight * FItem[I].Count;
  if (Result < 0) then
    Result := 0;
end;

function TInv.GetItemWeight(I: Integer): Integer;
begin
  Result := FItem[I].Weight;
  if (Result < 0) then
    Result := 0;
end;

function TInv.GetTough(I: Integer): Integer;
begin
  Result := FItem[I].Tough;
  if (Result < 0) then
    Result := 0;
end;

procedure TInv.SetTough(ID, ATough: Integer);
begin
  if (ATough < 0) then
    ATough := 0;
  FItem[ID].Tough := ATough;
end;

function TInv.GetDoll(I: Integer): Boolean;
begin
  Result := FItem[I].Doll;
end;

function TInv.GetStack(I: Integer): Boolean;
begin
  Result := FItem[I].Stack;
end;

procedure TInv.Clear(AMaxCount, AMaxWeight: Integer);
var
  I: 1 .. 26;
begin
  MaxCount := AMaxCount;
  MaxWeight := AMaxWeight;
  for I := 1 to 26 do
    Empty(I);
end;

procedure TInv.Clear;
var
  I: 1 .. 26;
begin
  for I := 1 to 26 do
    Empty(I);
end;

procedure TInv.Empty(I: Integer);
begin
  FItem[I].ID := '';
  FItem[I].Count := 0;
  FItem[I].Weight := 0;
  FItem[I].Tough := 0;
  FItem[I].Stack := False;
  FItem[I].Doll := False;
end;

function TInv.IndexOf(ID: string): Integer;
var
  I: 1 .. 26;
begin
  Result := 0;
  for I := 1 to 26 do
    if (FItem[I].ID = ID) then
    begin
      Result := I;
      Break;
    end;
  if (Result < 0) then
    Result := 0;
end;

function TInv.Count: Integer;
var
  I: 1 .. 26;
  FCount: Integer;
begin
  FCount := 0;
  for I := 1 to 26 do
    if (FItem[I].ID <> '') then
      FCount := FCount + 1;
  Result := FCount;
  if (Result < 0) then
    Result := 0;
end;

function TInv.Weight: Integer;
var
  I: 1 .. 26;
  FWeight: Integer;
begin
  FWeight := 0;
  for I := 1 to 26 do
    if (FItem[I].ID <> '') then
      FWeight := FWeight + (FItem[I].Weight * FItem[I].Count);
  Result := FWeight;
  if (Result < 0) then
    Result := 0;
end;

function TInv.Add(ID: string; ACount, AWeight, ATough: Integer;
  AStack: Boolean): Boolean;
var
  I: 1 .. 26;
begin
  Result := False;
  if (ACount <= 0) then
    Exit;
  I := IndexOf(ID);
  if (I = 0) or not AStack then
  begin
    I := IndexOf('');
    if (I = 0) then
      Exit;
  end;
  if (Self.Weight >= MaxWeight) then
    Exit;
  if (FItem[I].Count = 0) and (Self.Count >= MaxCount) then
    Exit;
  FItem[I].ID := ID;
  FItem[I].Weight := AWeight;
  FItem[I].Stack := AStack;
  FItem[I].Tough := ATough;
  FItem[I].Count := FItem[I].Count + ACount;
  Result := True;
end;

function TInv.Del(I, ACount: Integer): Boolean;
var
  J: 1 .. 26;
begin
  Result := False;
  if (FItem[I].Count = 0) or (FItem[I].Count < ACount) then
    Exit;

  FItem[I].Count := FItem[I].Count - ACount;
  if (FItem[I].Count = 0) then
    Empty(I);
  Result := True;

  for J := I to Count do
  begin
    if (FItem[J].ID = '') then
    begin
      FItem[J] := FItem[J + 1];
      Empty(J + 1);
    end;
  end;
end;

function TInv.Del(ID: string; ACount: Integer): Boolean;
var
  I: 1 .. 26;
begin
  Result := False;
  I := IndexOf(ID);
  if (I = 0) then
    Exit;
  Del(I, ACount);
end;

{ TAdvInv }

constructor TAdvInv.Create(AMaxCount, AMaxWeight: Integer);
begin
  inherited Create(AMaxCount, AMaxWeight);
  FF := TStringList.Create;
end;

constructor TAdvInv.Create;
begin
  Create(0, 0);
end;

destructor TAdvInv.Destroy;
begin
  FF.Free;
  inherited;
end;

procedure TAdvInv.Load;
var
  P, I: Integer;
  E: TExplodeResult;
begin
  P := 1;
  E := nil;
  Clear(MaxCount, MaxWeight);
  for I := 0 to FF.Count - 1 do
  begin
    E := Explode('/', FF[I]);
    if (Trim(E[0]) <> '') then
    begin
      FItem[P].ID := E[0];
      FItem[P].Count := StrToInt(E[1]);
      FItem[P].Weight := StrToInt(E[2]);
      FItem[P].Tough := StrToInt(E[3]);
      FItem[P].Stack := (FItem[P].Count > 1);
      FItem[P].Doll := ToBoo(E[4]);
    end;
    Inc(P);
  end;
end;

procedure TAdvInv.Save;
var
  I: 1 .. 26;
begin
  FF.Clear;
  for I := 1 to 26 do
    with FItem[I] do
      FF.Append(Format('%s/%d/%d/%d/%d', [ID, Count, Weight, Tough,
        ToInt(Doll)]));
end;

function TAdvInv.Equip(I: Integer): Boolean;
begin
  // Result := False;
  // if not FItem[I].Stack and (FItem[I].Count = 1) then
  begin
    FItem[I].Doll := True;
    Result := True;
  end;
end;

function TAdvInv.UnEquip(I: Integer): Boolean;
begin
  Result := False;
  if FItem[I].Doll { and not FItem[I].Stack and (FItem[I].Count = 1) } then
  begin
    FItem[I].Doll := False;
    Result := True;
  end;
end;

procedure TInv.SetMaxCount(Value: Integer);
begin
  if (Value < 0) then
    Value := 0;
  if (Value > 26) then
    Value := 26;
  FMaxCount := Value;
end;

procedure TInv.SetMaxWeight(Value: Integer);
begin
  if (Value < 0) then
    Value := 0;
  if (Value > 500) then
    Value := 500;
  FMaxWeight := Value;
end;

function TAdvInv.GetText: string;
begin
  Self.Save;
  Result := FF.Text;
end;

procedure TAdvInv.SetText(const Value: string);
begin
  FF.Text := Value;
  Self.Load;
end;

end.
