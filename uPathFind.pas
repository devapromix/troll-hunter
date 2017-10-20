unit uPathFind; // By KIPAR

interface

const
  MAXLEN = 1000;

  KNORM = 10;
  KDIAG = 12;
  KHEUR = 15;

type

  TSmallPoint = record
    X, Y: Word;
  end;

  TPathFindBlock = record
    // IsClosed :boolean;
    // OpenID :integer;
    CostWay { , CostHeur } : integer;
    Parent: TSmallPoint;
  end;

  TOpenBlock = record
    Cost, X, Y: integer;
  end;

  POpenBlock = ^TOpenBlock;

  TPathFindMap = array of TPathFindBlock;

  TGetXYVal = function(X, Y: integer): boolean; stdcall;

var
  Cells: TPathFindMap;
  FAULT: integer;
  SavedMapX, SavedMapY: integer;
  { TOTAL, CHECKED: integer; }

function PathFind(MapX, MapY, FromX, FromY, ToX, ToY: integer;
  Callback: TGetXYVal; var TargetX, TargetY: integer): boolean;

procedure InitCrap;

implementation

function Max(A, B: integer): integer; inline;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: integer): integer; inline;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function InRange(X, A, B: integer): boolean; inline;
begin
  Result := (X >= A) and (X <= B);
end;

var
  Open: array [0 .. MAXLEN] of POpenBlock;
  OpenRaw: array [0 .. MAXLEN] of TOpenBlock;

procedure InitCrap;
var
  I: integer;
begin
  for I := 0 to MAXLEN do
  begin
    Open[I] := @OpenRaw[I];
    { OpenRaw[I].X := 0;
      OpenRaw[I].Y := 0;
      OpenRaw[I].Cost := 0; }
  end;

end;

function Heuristic(dx, dy: integer): integer; inline;
begin
  // Result := KHEUR * Max(dx, dy);
  Result := KNORM * Max(dx, dy) + (KDIAG - KNORM) * Min(dx, dy);
end;

var
  NOpen: integer = 0;

function PathFind(MapX, MapY, FromX, FromY, ToX, ToY: integer;
  Callback: TGetXYVal; var TargetX, TargetY: integer): boolean;

  procedure HeapSwap(I, j: integer); inline;
  var
    tmp: POpenBlock;
  begin
    tmp := Open[I];
    Open[I] := Open[j];
    Open[j] := tmp;
  end;

  procedure HeapAdd; inline;
  var
    I, Parent: integer;
  begin
    I := NOpen - 1;
    Parent := (I - 1) div 2;
    while (I > 0) and (Open[Parent].Cost > Open[I].Cost) do
    begin
      HeapSwap(I, Parent);
      I := Parent;
      Parent := (I - 1) div 2;
    end;
  end;

  procedure Heapify(I: integer); inline;
  var
    leftChild, rightChild, largestChild: integer;
  begin
    repeat
      leftChild := 2 * I + 1;
      if leftChild >= NOpen then
        exit;
      rightChild := leftChild + 1;
      largestChild := I;
      if Open[leftChild].Cost < Open[largestChild].Cost then
        largestChild := leftChild;
      if (rightChild < NOpen) and
        (Open[rightChild].Cost < Open[largestChild].Cost) then
        largestChild := rightChild;
      if largestChild = I then
        exit;
      HeapSwap(I, largestChild);
      I := largestChild;
    until false;
  end;

  procedure AddToOpen(X, Y, FrX, FrY, NewCost: integer);
  begin
    if not InRange(X, 0, MapX - 1) then
      exit;
    if not InRange(Y, 0, MapY - 1) then
      exit;
    with Cells[X * MapY + Y] do
    begin
      if CostWay > 0 then // if OpenID > 0 then
      begin
        // if CostWay <= NewCost then
        exit;
      end;
      if not Callback(X, Y) then
        exit;
      if NOpen >= MAXLEN then
        exit;
      Open[NOpen].X := X;
      Open[NOpen].Y := Y;
      // TODO??
      CostWay := NewCost;
      Open[NOpen].Cost := CostWay + Heuristic(abs(X - FromX), abs(Y - FromY));
      Inc(NOpen);
      HeapAdd;
      Parent.X := FrX;
      Parent.Y := FrY;
    end;
  end;

var
  CurX, CurY: integer;
begin
  Result := false;
  if not InRange(ToX, 0, MapX - 1) then
    exit;
  if not InRange(ToY, 0, MapY - 1) then
    exit;
  if not Callback(ToX, ToY) then
    exit;
  // if not Callback(FromX, FromY) then exit;
  if (FromX = ToX) and (FromY = ToY) then
  begin
    Result := True;
    TargetX := ToX;
    TargetY := ToY;
    exit;
  end;

  if (SavedMapX <> MapX) or (SavedMapY <> MapY) then
  begin
    SetLength(Cells, 0);
    SetLength(Cells, MapX * MapY);
    SavedMapX := MapX;
    SavedMapY := MapY;
  end
  else
  begin
    FillChar(Pointer(Cells)^, MapX * MapY * Sizeof(Cells[0]), 0);
  end;
  // exit;

  NOpen := 0;
  // FillChar(Cells, SizeOf(Cells), 0);
  AddToOpen(ToX, ToY, -1, -1, 0);
  repeat
    CurX := Open[0].X;
    CurY := Open[0].Y;
    if (CurX = FromX) and (CurY = FromY) then
    begin
      Result := True;
      // Inc(TOTAL, Open[0].Cost);
      with Cells[CurX * MapY + CurY] do
      begin
        TargetX := Parent.X;
        TargetY := Parent.Y;
      end;
      exit;
    end;
    with Cells[CurX * MapY + CurY] do
    begin
      // IsClosed := True;
      // inc(CHECKED);
      HeapSwap(0, NOpen - 1);
      // Open[0] := Open[NOpen-1];
      Dec(NOpen);
      Heapify(0);
      AddToOpen(CurX - 1, CurY, CurX, CurY, CostWay + KNORM);
      AddToOpen(CurX, CurY - 1, CurX, CurY, CostWay + KNORM);
      AddToOpen(CurX, CurY + 1, CurX, CurY, CostWay + KNORM);
      AddToOpen(CurX + 1, CurY, CurX, CurY, CostWay + KNORM);
      AddToOpen(CurX - 1, CurY - 1, CurX, CurY, CostWay + KDIAG);
      AddToOpen(CurX - 1, CurY + 1, CurX, CurY, CostWay + KDIAG);
      AddToOpen(CurX + 1, CurY - 1, CurX, CurY, CostWay + KDIAG);
      AddToOpen(CurX + 1, CurY + 1, CurX, CurY, CostWay + KDIAG);
      if NOpen > FAULT then
        FAULT := NOpen;
    end;
  until NOpen <= 0;
  Result := false;
end;

initialization

InitCrap;

end.
