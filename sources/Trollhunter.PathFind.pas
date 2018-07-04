unit Trollhunter.PathFind; // By KIPAR

interface

uses Trollhunter.Types;

type
  TGetXYVal = function(X, Y: Int): Boolean; stdcall;

function PathFind(MapX, MapY, FromX, FromY, ToX, ToY: Int; Callback: TGetXYVal;
  var TargetX, TargetY: Int): Boolean;

implementation

uses Math;

const
  MAXLEN = 1000;
  KNORM = 10;
  KDIAG = 12;

type
  TUIntPoint = record
    X, Y: UInt;
  end;

type
  TPathFindBlock = record
    CostWay: Int;
    Parent: TUIntPoint;
  end;

type
  TOpenBlock = record
    Cost, X, Y: Int;
  end;

  POpenBlock = ^TOpenBlock;

  TPathFindMap = array of TPathFindBlock;

var
  Cells: TPathFindMap;
  Fault: Int;
  SavedMapX, SavedMapY: Int;
  Open: array [0 .. MAXLEN] of POpenBlock;
  OpenRaw: array [0 .. MAXLEN] of TOpenBlock;

procedure InitCrap;
var
  I: Int;
begin
  for I := 0 to MAXLEN do
    Open[I] := @OpenRaw[I];
end;

function Heuristic(DX, DY: Int): Int;
begin
  Result := KNORM * Max(DX, DY) + (KDIAG - KNORM) * Min(DX, DY);
end;

var
  NOpen: Int = 0;

function PathFind(MapX, MapY, FromX, FromY, ToX, ToY: Int; Callback: TGetXYVal;
  var TargetX, TargetY: Int): Boolean;

  procedure HeapSwap(I, J: Int);
  var
    Tmp: POpenBlock;
  begin
    Tmp := Open[I];
    Open[I] := Open[J];
    Open[J] := Tmp;
  end;

  procedure HeapAdd;
  var
    I, Parent: Int;
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

  procedure Heapify(I: Int);
  var
    LeftChild, RightChild, LargestChild: Int;
  begin
    repeat
      LeftChild := 2 * I + 1;
      if LeftChild >= NOpen then
        Exit;
      RightChild := LeftChild + 1;
      LargestChild := I;
      if Open[LeftChild].Cost < Open[LargestChild].Cost then
        LargestChild := LeftChild;
      if (RightChild < NOpen) and
        (Open[RightChild].Cost < Open[LargestChild].Cost) then
        LargestChild := RightChild;
      if LargestChild = I then
        Exit;
      HeapSwap(I, LargestChild);
      I := LargestChild;
    until False;
  end;

  procedure AddToOpen(X, Y, FrX, FrY, NewCost: Int);
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
  CurX, CurY: Int;
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
