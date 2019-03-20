unit PathFind; // A* by KIPAR

interface

type
  TGetXYVal = function(X, Y: Integer): Boolean; stdcall;

function IsPathFind(MapX, MapY, FromX, FromY, ToX, ToY: Integer; Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;

implementation

uses
  Math;

const
  MAXLEN = 1000;
  KNORM = 10;
  KDIAG = 12;

type
  TUIntPoint = record
    X, Y: Word;
  end;

type
  TPathFindBlock = record
    CostWay: Integer;
    Parent: TUIntPoint;
  end;

type
  TOpenBlock = record
    Cost, X, Y: Integer;
  end;

  POpenBlock = ^TOpenBlock;

  TPathFindMapArray = TArray<TPathFindBlock>;
  TOpenBlockArray<T> = array [0 .. MAXLEN] of T;

var
  Cells: TPathFindMapArray;
  Fault: Integer;
  SavedMapX, SavedMapY: Integer;
  Open: TOpenBlockArray<POpenBlock>;
  OpenRaw: TOpenBlockArray<TOpenBlock>;

procedure InitCrap;
var
  I: Integer;
begin
  for I := 0 to MAXLEN do
    Open[I] := @OpenRaw[I];
end;

function Heuristic(DX, DY: Integer): Integer;
begin
  Result := KNORM * Max(DX, DY) + (KDIAG - KNORM) * Min(DX, DY);
end;

var
  NOpen: Integer = 0;

function IsPathFind(MapX, MapY, FromX, FromY, ToX, ToY: Integer; Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;

  procedure HeapSwap(I, J: Integer);
  var
    Tmp: POpenBlock;
  begin
    Tmp := Open[I];
    Open[I] := Open[J];
    Open[J] := Tmp;
  end;

  procedure HeapAdd;
  var
    I, Parent: Integer;
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

  procedure Heapify(I: Integer);
  var
    LeftChild, RightChild, LargestChild: Integer;
  begin
    repeat
      LeftChild := 2 * I + 1;
      if LeftChild >= NOpen then
        Exit;
      RightChild := LeftChild + 1;
      LargestChild := I;
      if Open[LeftChild].Cost < Open[LargestChild].Cost then
        LargestChild := LeftChild;
      if (RightChild < NOpen) and (Open[RightChild].Cost < Open[LargestChild].Cost) then
        LargestChild := RightChild;
      if LargestChild = I then
        Exit;
      HeapSwap(I, LargestChild);
      I := LargestChild;
    until False;
  end;

  procedure AddToOpen(X, Y, FrX, FrY, NewCost: Integer);
  begin
    if not InRange(X, 0, MapX - 1) or not InRange(Y, 0, MapY - 1) then
      Exit;
    with Cells[X * MapY + Y] do
    begin
      if CostWay > 0 then // if OpenID > 0 then
      begin
        // if CostWay <= NewCost then
        Exit;
      end;
      if not Callback(X, Y) then
        Exit;
      if NOpen >= MAXLEN then
        Exit;
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
  CurX, CurY: Integer;
begin
  Result := False;
  if not InRange(ToX, 0, MapX - 1) or not InRange(ToY, 0, MapY - 1) then
    Exit;
  if not Callback(ToX, ToY) then
    Exit;
  // if not Callback(FromX, FromY) then exit;
  if (FromX = ToX) and (FromY = ToY) then
  begin
    Result := True;
    TargetX := ToX;
    TargetY := ToY;
    Exit;
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
      Exit;
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
      if NOpen > Fault then
        Fault := NOpen;
    end;
  until NOpen <= 0;
  Result := False;
end;

initialization

InitCrap;

end.
