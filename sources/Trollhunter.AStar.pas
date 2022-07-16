unit Trollhunter.AStar;

interface

uses
  Types,
  Trollhunter.Utils;

const
  MAXLEN = 500;
  KNORM = 10;
  KDIAG = 12;
  KHEUR = 15;

type
  TPathFindBlock = record
    IsClosed: Boolean;
    OpenID: Integer;
    CostWay, CostHeur: Integer;
    Parent: TPoint;
  end;

  TPathFindMap = array [1 .. MapSide, 1 .. MapSide] of TPathFindBlock;
  TGetXYVal = function(X, Y: Integer): Boolean; stdcall;

var
  Cells: TPathFindMap;

function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer;
  Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;

implementation

function DoAStar(MapX, MapY, FromX, FromY, ToX, ToY: Integer;
  Callback: TGetXYVal; var TargetX, TargetY: Integer): Boolean;

type
  TMLen = array [1 .. MAXLEN] of Integer;

var
  OpenX, OpenY, OpenCost: TMLen;
  NOpen: Integer;

  function Heuristic(X, Y: Integer): Integer;
  begin
    Result := KHEUR * Max(Abs(X - FromX), Abs(Y - FromY));
  end;

  procedure AddToOpen(X, Y, FrX, FrY, NewCost: Integer);
  begin
    if not InRange(X, 1, MapX) or not InRange(Y, 1, MapY) then
      Exit;
    with Cells[X, Y] do
    begin
      if ((X <> ToX) or (Y <> ToY)) and not Callback(X, Y) then
        Exit;
      if OpenID > 0 then
      begin
        if CostWay <= NewCost then
          Exit;
      end
      else
      begin
        if NOpen = MAXLEN then
          Exit;
        Inc(NOpen);
        OpenID := NOpen;
        OpenX[NOpen] := X;
        OpenY[NOpen] := Y;
      end;
      CostWay := NewCost;
      CostHeur := Heuristic(X, Y);
      OpenCost[OpenID] := CostWay + CostHeur;
      Parent.X := FrX;
      Parent.Y := FrY;
    end;
  end;

var
  BestID, MinC, I, CurX, CurY: Integer;

begin
  Result := False;
  if not InRange(ToX, 1, MapX) or not InRange(ToY, 1, MapY) then
    Exit;
  NOpen := 0;
  FillChar(Cells, SizeOf(Cells), 0);
  AddToOpen(ToX, ToY, -1, -1, 0);
  repeat
    MinC := MAXINT;
    BestID := -1;
    for I := 1 to NOpen do
      if OpenCost[I] < MinC then
      begin
        BestID := I;
        MinC := OpenCost[I];
      end;
    CurX := OpenX[BestID];
    CurY := OpenY[BestID];
    if (CurX = FromX) and (CurY = FromY) then
    begin
      Result := True;
      with Cells[CurX, CurY] do
      begin
        TargetX := Parent.X;
        TargetY := Parent.Y;
      end;
      Exit;
    end;
    with Cells[CurX, CurY] do
    begin
      IsClosed := True;
      OpenX[BestID] := OpenX[NOpen];
      OpenY[BestID] := OpenY[NOpen];
      OpenCost[BestID] := OpenCost[NOpen];
      Dec(NOpen);
      AddToOpen(CurX - 1, CurY - 1, CurX, CurY, CostWay + KDIAG);
      AddToOpen(CurX - 1, CurY, CurX, CurY, CostWay + KNORM);
      AddToOpen(CurX - 1, CurY + 1, CurX, CurY, CostWay + KDIAG);
      AddToOpen(CurX, CurY - 1, CurX, CurY, CostWay + KNORM);
      AddToOpen(CurX, CurY + 1, CurX, CurY, CostWay + KNORM);
      AddToOpen(CurX + 1, CurY - 1, CurX, CurY, CostWay + KDIAG);
      AddToOpen(CurX + 1, CurY, CurX, CurY, CostWay + KNORM);
      AddToOpen(CurX + 1, CurY + 1, CurX, CurY, CostWay + KDIAG);
    end;
  until (NOpen = 0);
  Result := False;
end;

end.
