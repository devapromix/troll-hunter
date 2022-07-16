{
  BeaRLib, base map generation algorithms.
  Authors\maintainers: JustHarry\Apromix
  ***CAVES\ANTNEST ALGORITHM AUTOR - JAKOB DEBSKI***
}

unit Trollhunter.Map.Generator;

interface

type TBeaRLibMap = array of Char;
procedure CreateMap(X, Y, ID: Integer; var A: TBeaRLibMap; S: Integer);

const G_ANT_NEST=1;
const G_CAVES = 2;
const G_VILLAGE = 3;
const G_LAKES = 4;
const G_LAKES2 = 5;
const G_TOWER = 6;
const G_HIVE = 7;
const G_CITY = 8;
const G_MOUNTAIN=9;
const G_FOREST = 10;
const G_SWAMP = 11;
const G_RIFT=12;
const G_TUNDRA=13;
const G_BROKEN_CITY=14;
const G_BROKEN_VILLAGE=15;
const G_DARK_ROOMS=16;
const G_DOOM_ROOMS=17;   {!}
const G_DARK_CAVE=18;
const G_DARK_GROTTO=19;
const G_RED_ROOMS=20;
const G_DARK_FOREST=21;  {!}
const G_STONY_FIELD=22;

const MAXID = 22;

const TILE_WALL=0;
const TILE_FLOOR=1;
const TILE_WATER = 2;
const TILE_TREE = 3;
const TILE_BUSH = 4;
const TILE_GRASS = 5; 
const TILE_MOUNTAIN = 6;
const TILE_DOOR=7;
const TILE_ROAD=8;
const TILE_EMPTY=9;
const TILE_OPEN_DOOR=10;
const TILE_IN=11;
const TILE_OUT=12;
const TILE_STONE=13;

const TILE_LAST=13;

implementation

uses Windows, SysUtils;

const
  MaxX = 512 - 1;
  MaxY = 512 - 1;
  MinX = 50 - 1;
  MinY = 50 - 1;


type
  TRect = record
    X, Y, W, H: Integer;
  end;
  
  Tile = record
    Ch: Char;
    Color: Byte;
  end;

  StdArray = array[-100..MAXX + 100, -100..MAXY + 100] of Byte;

var
  Map: StdArray;
  MapX, MapY: Integer;

const tileset: array[0..TILE_LAST] of Tile =(
(ch:'#';color:7),
(ch:'.';color:7),
(ch:'~';color:1),
(ch:'T';color:10),
(ch:'t';color:10),
(ch:',';color:10),
(ch:'^';color:8),
(ch:'+';color:7),
(ch:'%';color:8),
(ch:' ';color:0),
(ch:'-';color:0),
(ch:'<';color:0),
(ch:'>';color:0),
(ch:';';color:1)
);

procedure Box(const BoxStrMessage: string);
begin
  MessageBox(0, PChar(BoxStrMessage), 'BeaRLibMG', MB_OK);
end;

function Min(const A, B: Integer): Integer;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Integer): Integer;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Sign(x:integer):integer;
begin
    if x>0 then Sign:=1 else
    if x<0 then Sign:=-1 else
    Sign:=0;
end;

function Rand(A, B: Integer): Integer;
begin
  Result := Round(Random(B - A + 1) + A);
end;

function FillRegion(AX, AY, SX, SY, Tile: Integer; Check: Boolean = True): Boolean;
var
  X, Y, X1, Y1, X2, Y2: Integer;
begin
  Result := False;
  X1 := AX - SX;
  Y1 := AY - SY;
  X2 := AX + SX;
  Y2 := AY + SY;
  if Check then
    for Y := Y1 - 1 to Y2 + 1 do
      for X := X1 - 1 to X2 + 1 do
        if (Map[X, Y] <> TILE_WALL) then Exit;
  for Y := Y1 to Y2 do
    for X := X1 to X2 do
      Map[X, Y] := Tile;
  Result := True;          
end;

procedure DrawLine(x1,y1,x2,y2,id:integer);
    var dx,dy,i,sx,sy,check,e,x,y:integer;
    begin
        dx:=abs(x1-x2);
        dy:=abs(y1-y2);
        sx:=Sign(x2-x1);
        sy:=Sign(y2-y1);
        x:=x1;
        y:=y1;
        check:=0;
        if dy>dx then begin
            dx:=dx+dy;
            dy:=dx-dy;
            dx:=dx-dy;
            check:=1;
        end;
        e:= 2*dy - dx;
        for i:=0 to dx do begin
            Map[x,y]:=id;
            if e>=0 then begin
                if check=1 then x:=x+sx else y:=y+sy;
                e:=e-2*dx;
            end;
            if check=1 then y:=y+sy else x:=x+sx;
            e:=e+2*dy;
        end;
    end;

function countnearby(x,y,id:integer):integer;
 var res:integer;
begin
  res:=0;
  if map[x-1,y]=id then res:=res+1;
  if map[x+1,y]=id then res:=res+1;
  if map[x,y-1]=id then res:=res+1;
  if map[x,y+1]=id then res:=res+1;
  if map[x-1,y-1]=id then res:=res+1;
  if map[x-1,y+1]=id then res:=res+1;
  if map[x+1,y+1]=id then res:=res+1;
  if map[x+1,y+1]=id then res:=res+1;
  countnearby:=res;
end;

procedure MapClear(ID: Integer);
var
  I, J:integer;
begin
  for I := -100 to MAXX + 100 do
    for J := -100 to MAXX + 100 do
      Map[I, J] := ID;
end;

function Dist(x1,y1,x2,y2:integer):integer;
begin
  Result := round(sqrt(sqr(x2-x1)+sqr(y2-y1)));
end;

function FreeSpace(x1,y1,x2,y2:integer):boolean;
 var i,j:integer;
begin
 dec(x1);
 inc(x2);
 dec(y1); 
 inc(y2);
 freespace:=true;
 for i:=x1 to x2 do
  for j:=y1 to y2 do
   if ((Tileset[map[i,j]].ch<>'.')) then freespace:=false;
end;


function ReturnPlaceCoord(minvalue,maxvalue:integer; var nx,ny,lenx,leny:integer):boolean;
var i,j,newx,newy,count:integer;
begin
ReturnPlaceCoord:=false;
count:=0;
  i:=random(MapX-maxvalue-2)+2;
  j:=random(MapY-maxvalue-2)+2;
  newx:=random(maxvalue-minvalue)+1+minvalue;
  newy:=random(maxvalue-minvalue)+1+minvalue;
  while not(freespace(i,j,i+newx,j+newy)) do
    begin
       i:=random(MapX-maxvalue-2)+2;
       j:=random(MapY-maxvalue-2)+2;
       newx:=random(maxvalue-minvalue)+1+minvalue;
       newy:=random(maxvalue-minvalue)+1+minvalue;
       inc(count);
       if count>100 then exit;
    end;
nx:=i;
ny:=j;
lenx:=newx;
leny:=newy;
ReturnPlaceCoord:=true;
end;

function TileDoor(): Integer;
begin
  Result := TILE_DOOR;
  if (Random(15) <= 1) then Result := TILE_OPEN_DOOR;
end;

procedure LakesCreate(x1,y1,x2,y2,typ:integer);
const density = 0.60;
  var i,j:integer;
  var res:integer;
  var x,y:integer;
begin
x:=x2-x1+1;
y:=y2-y1+1;
 for i:=1 to round(X*Y*density) do
    Map[x1+random(X),y1+random(Y)]:=TILE_WALL;
 for i:=x1 to X2 do
  for j:=y1 to Y2 do
   begin
    if (i=x1) or (j=x1) or (i=X2) or (j=Y2) then
       begin
         map[i,j]:=TILE_FLOOR;
         continue;
       end;
   res:=countnearby(i,j,TILE_WALL);
     if (map[i,j]=TILE_WALL) then
       begin
         if res<4 then
            map[i,j]:=TILE_FLOOR;
       end
     else
        begin
         if res>4 then
          map[i,j]:=TILE_WALL;
       end
   end;
for i:=x1 to X2 do
   for j:=y1 to Y2 do
    if countnearby(i,j,TILE_FLOOR)<3 then
     map[i,j]:=TILE_WALL;
for res:=1 to 10 do
  for i:=x1 to X2 do
   for j:=y1 to Y2 do
    if (countnearby(i,j,TILE_FLOOR)>=7) then
     map[i,j]:=TILE_FLOOR;

 for i:=x1 to X2 do
  for j:=y1 to Y2 do
     if typ=0 then
     begin
      if map[i,j]=TILE_WALL  then
        map[i,j]:=TILE_FLOOR
      else
        map[i,j]:=TILE_WATER;
     end
       else
     begin
     if map[i,j]=TILE_WALL  then
       map[i,j]:=TILE_WATER
     else
       map[i,j]:=TILE_FLOOR;
     end
end;

function GetTree(): Integer;
begin
  if (Rand(1, 2) = 1) then Result := TILE_TREE else Result := TILE_BUSH;
end;


procedure ForestPartDraw(x1,y1:integer);
var
  n,s,e,w,i,j,k: integer;
begin
  i:= x1;
  j:= y1;
  for k:= 1 to 20 do begin
    n:= random(6);
    e:= random(6);
    s:= random(6);
    w:= random(6);
    if n = 1 then begin
      i:= i - 1;
      if Tileset[map[i,j]].ch<>'.' then exit;
      map[i,j]:= GetTree();
    end;
    if s = 1 then begin
      i:= i + 1;
      if Tileset[map[i,j]].ch<>'.' then exit;
      map[i,j]:= GetTree();
    end;
    if e = 1 then begin
      j:= j + 1;
      if Tileset[map[i,j]].ch<>'.' then exit;
      map[i,j]:= GetTree();
    end;
    if w = 1 then begin
      j:= j - 1;
      if Tileset[map[i,j]].ch<>'.' then exit;
      map[i,j]:= GetTree();
    end;
  end;
end;

procedure AddModOnMap(modtype:integer);
 var i,j:integer;
begin
case modtype of
 1:
   begin
    for i:=1 to MapX do
      for j:=1 to MapY do
        if map[i,j]=TILE_WALL then
         if random(100)<=40 then
          map[i,j]:=TILE_FLOOR;

   end;
 2:
    for i:=1 to MapX*MapY div 25 do
      ForestPartDraw(random(MapX)+1,random(MapY)+1);
 3:
   for i:=1 to MapX do
     for j:=1 to MapY do
      if random(100)<=20 then
        map[i,j]:=TILE_WATER;
end;

end;


procedure ForestCreate(x,y:integer);
var i:integer;
begin
 for i:=1 to x*y div 15 do
   ForestPartDraw(random(x)+1,random(y)+1);
end;


procedure AntNestCreate(x1,y1,x2,y2,typ:integer);
var
  i,j:integer;
  kx,ky,k,dx,dy:real;
  x,y,py, px:integer;
  counter: integer;
  buffer:stdarray;
begin
  x:=x2-x1+1;
  y:=y2-y1+1;
  buffer:=map;
//  MapClear(TILE_FLOOR);
  Map[X div 2, Y div 2] := TILE_WALL;
  for i:=0 to (X*Y div 3) do
  begin
   try
   k := (random(360)+1)*3.1419532/180;
   kx := (X/2) + (Y/2)*sin(k);
   ky := (Y/2) + (Y/2)*cos(k);
   dx := 1;
   dy := 1;
   while ((abs(dx)<10) and (abs(dy)<10)) do
    begin
     dx := Random(100)+1;
     dy := Random(100)+1;
    end;
   dx := dx - 60;
   dy := dy - 60;
   dx := dx / 30;
   dy := dy / 30;
   counter := 0;
   while (true) do
   begin
    if counter+1>10000  then
        break;
    counter := counter +1;
    kx := kx + dx;
    ky := ky + dy;
    px := round(kx);
    py := round(ky);
   if (px<0) then
      begin
       px := X;
       kx := px;
      end;
    if (px>X) then
      begin
       px := 1;
       kx := px;
      end;
    if (py<0) then
      begin
       py := Y;
       ky := py;
      end;
    if (py>Y) then
      begin
       py := 1;
       ky := py;  
      end;
      if (px=0) then px := random(x)+1;
      if (py=0) then py := random(y)+1;
    if ((px>1)  and  (Map[px-1,py]=TILE_WALL)) or
       ((py>1)  and  (Map[px,py-1]=TILE_WALL)) or
       ((px<X)  and  (Map[px+1,py]=TILE_WALL)) or
       ((py<Y)  and  (Map[px,py+1]=TILE_WALL)) then
     begin
      Map[px,py]:=TILE_WALL;
      Break;
     end;
   end;
   except end;
  end;


for i:=1 to X do
   for j:=1 to Y do
     if map[i,j]=TILE_WALL  then
       map[i,j]:=TILE_FLOOR
     else
       map[i,j]:=TILE_WALL;

if typ>0 then
for i:=2 to X-1 do
 for j:=2 to Y-1 do
 if countnearby(i,j,TILE_WALL)<=3 then
  map[i,j]:=TILE_FLOOR;
if typ=2 then
for i:=1 to X do
 for j:=1 to Y do
 if map[i,j]=TILE_WALL then
  map[i,j]:=TILE_FLOOR
  else
 if map[i,j]=TILE_FLOOR then
   map[i,j] := TILE_MOUNTAIN;
for i:=x1 to x2 do
  for j:=y1 to y2 do
    buffer[i,j]:=map[i-x1+1,j-y1+1];
  map:=buffer;
  
end;



procedure RiftCreate(x,y:integer);
 var i,j:integer;
begin
 for i:=1 to x*y div 10 do
   ForestPartDraw(random(x)+1,random(y)+1);
 for i:=1 to X do
  for j:=1 to Y do
   if map[i,j]=TILE_TREE then
    map[i,j]:=TILE_EMPTY
   else
    map[i,j]:=TILE_MOUNTAIN;
end;

procedure TundraCreate(x,y:integer);
 var i,newx,newy,lenx,leny:integer;
begin
 for i:=1 to x*y div 100 do
   ForestPartDraw(random(x)+1,random(y)+1);
 for i:=1 to x*y div 100 do
  begin
   if not(ReturnPlaceCoord(10,15,newx,newy,lenx,leny)) then continue;
   if random(100)<=50 then
    AntNestCreate(newx,newy,newx+lenx,newy+leny,2)
   else
    LakesCreate(newx,newy,newx+lenx,newy+leny,1);
  end;
end;

procedure DrawRoom(x1,y1,x2,y2:integer);
var i,x,y,k:integer;
    gh:boolean;
begin
for i:=x1 to x2 do
 begin
 if map[i,y1]<>TILE_DOOR then
  map[i,y1]:=TILE_WALL;
 if map[i,y2]<>TILE_DOOR then
  map[i,y2]:=TILE_WALL;
 end;
for i:=y1 to y2 do
 begin
if map[x1,i]<>TILE_DOOR then
  map[x1,i]:=TILE_WALL;
 if map[x2,i]<>TILE_DOOR then
  map[x2,i]:=TILE_WALL;
 end;
for i:=1 to 30 do
while true do
begin
gh:=false;
x:=random(x2-x1-1)+1 +i -i;
y:=random(y2-y1-1)+1;
k:=random(5);
if k=0 then
if countnearby(x1+x,y1,TILE_DOOR)=0 then
if map[x1+x,y1]<>TILE_DOOR then
begin
   map[x1+x,y1]:=TileDoor();
   gh:=true;
end;
if k=1 then
if map[x1,y+y1]<>TILE_DOOR then
if countnearby(x1,y1+y,TILE_DOOR)=0 then
begin
   map[x1,y1+y]:=TileDoor();
   gh:=true;
end;
if k=2 then
if map[x2,y1+y]<>TILE_DOOR then
if countnearby(x2,y1+y,TILE_DOOR)=0 then
begin
   map[x2,y1+y]:=TileDoor();
   gh:=true;
end;
if k=3 then
if map[x1+x,y2]<>TILE_DOOR then
if countnearby(x1+x,y2,TILE_DOOR)=0 then
begin
   map[x1+x,y2]:=TileDoor();
   gh:=true;
end;
if gh then exit;
end;
end;

procedure CreateRoom(X1, Y1, X2, Y2: Integer; R: Integer = 0);
 const MaxRoomValue = 3;
 var count,j:integer;
begin
 if (x2-x1)<maxroomvalue*2 then exit;
 if (y2-y1)<maxroomvalue*2 then exit;
 DrawRoom(x1,y1,x2,y2);
 if (R = 0) then
   begin
     j:=0;
     count:=0;
     while (j<maxroomvalue) or (x2-(x1+j)<maxroomvalue)  do
      begin
        j:=random(x2-x1)+1;
        inc(count);
        if count>100 then exit;
      end;
     CreateRoom(x1,y1,x1+j,y2, -1);
     CreateRoom(x1+j,y1,x2,y2, -1);
     exit;
   end
   else
   begin
     j:=0;
     count:=0;
     while (j<maxroomvalue) or (y2-(y1+j)<maxroomvalue)  do
      begin
        j:=random(y2-y1)+1;
        inc(count);
        if count>100 then exit;
      end;
     CreateRoom(x1,y1,x2,y1+j);
     CreateRoom(x1,y1+j,x2,y2);
    end;
end;

procedure CreateHouse(x1,y1,x2,y2:integer);
begin
if (x2-x1+1<=8) or (y2-y1+1<=8) then
  DrawRoom(x1,y1,x2,y2)
else
  CreateRoom(x1,y1,x2,y2);
end;

procedure CreateSomething(minvalue,maxvalue:integer;flag:boolean);
var
  i, j, count, newx, newy: integer;
begin
if not(ReturnPlaceCoord(minvalue,maxvalue,i,j,newx,newy)) then exit;
      if flag=true then
      begin
        if random(100)<=25 then
          for count:=1 to 10 do
            ForestPartDraw(random(newx)+i,random(newy)+j)
        else if random(100)<=20 then
          LakesCreate(i,j,i+newx,j+newy,1)
          else CreateHouse(i,j,i+newx,j+newy);
      end
        else CreateHouse(i,j,i+newx,j+newy);
end;

procedure VillageCreate(X,Y,typ:integer);
var k:integer;
begin
 for k:=1 to 100 do
   CreateSomething(10,15,true);
 if typ=1 then
   begin
    AddModOnMap(1);
    AddModOnMap(2);

   end;
end;


procedure TowerCreate(X,Y:integer);
const waterch = 20;
var px,py,rad:integer;
    k,i,j:integer;
begin
  px:=x div 2;
  py:=y div 2;
  if (Y-py)<(X-px) then rad:=y-py else
          rad:=x-px;
  k:=rad-5;
  while k>0 do
   begin
     for i:=1 to X do
      for j:=1 to Y do
       if dist(i,j,px,py)=k then
        map[i,j]:=TILE_WALL;
   k:=k-2;
   end;
 AddModOnMap(1);
 AddModOnMap(1);
 AddModOnMap(2);
 AddModOnMap(3);
end;

procedure SwampCreate(X,Y:integer);
var
  i,j:integer;
  buffer: stdarray;
begin
  for i:=1 to X*Y div 1000 do
    CreateSomething(5,10,false);
for i:=1 to X*Y div 20 do
  ForestPartDraw(random(x)+1, random(y)+1);
buffer:=map;
LakesCreate(1,1,X,Y,0);
for i:=1 to X do
 for j:=1 to Y do
  if buffer[i,j]=TILE_WALL then
    map[i,j]:=TILE_WALL
  else
    if buffer[i,j]=TILE_TREE then
    map[i,j]:=TILE_TREE;
AddModOnMap(1);

end;

procedure CavesCreate(X,Y:integer);
const density = 0.65;
  var i,j:integer;
  var res:integer;
begin
 for i:=1 to round(X*Y*density) do
    Map[random(X)+1,random(Y)+1]:=TILE_WALL;
 for i:=1 to X do
  for j:=1 to Y do
   begin
    if (i<=1) or (j<=1) or (i>=X-1) or (j>=Y-1) then
       begin
         map[i,j]:=TILE_WALL;
         continue;
       end;
   res:=countnearby(i,j,TILE_WALL);
     if (map[i,j]=TILE_WALL) then
       begin
         if res<4 then
            map[i,j]:=TILE_FLOOR;
       end
     else
        begin
         if res>4 then
          map[i,j]:=TILE_WALL;
       end
   end;
 for res:=1 to 10 do
  for i:=2 to X-1 do
   for j:=2 to Y-1 do
    if (countnearby(i,j,TILE_FLOOR)<3) or (countnearby(i,j,TILE_WALL)>=7) then
     map[i,j]:=TILE_WALL;
end;


procedure CityCreate(X,Y,typ:integer);
var px,py,resx,i,j,k:integer;
var buffer:stdarray;
begin
CreateHouse(1,1,X div 4, Y div 4);
px:=1;
py:=1;
for i:=1 to X div 4 do
 for j:=1 to Y div 4 do
  begin
    for k:=1 to 4 do
     begin
      buffer[px,py]:=map[i,j];
      py:=py+1;
      if py>Y then
       begin
        px:=px+1;
        py:=1;
       end;
     end;
  end;
resx:=0;
for i:=1 to X div 4 do
for k:=1 to 4 do
begin
inc(resx);
 for j:=1 to Y do
  map[resx,j]:=buffer[i,j];
 end;
for i:=1 to X do
 for j:=1 to Y do
  if (map[i,j]=TILE_DOOR) or (map[i,j]=TILE_WALL) then
    map[i,j]:=TILE_ROAD;
for i:=1 to 1000 do
 CreateSomething(5,20,false);
if typ=1 then
  begin
    AddModOnMap(1);
    AddModOnMap(3);
  end;

end;

// #16
procedure DarkRoomsCreate(AX, AY: Integer);
var
  I, J: Integer;
  V: array of TRect;
  RoomsCount: Integer;
  P, L: TRect;

  procedure LinkRoom(X1, Y1, X2, Y2: Integer);
  var
    B, I, L, AX, AY, LX, LY: Integer;
    R: Real;
  begin
    B := 0;
    LX := 0;
    LY := 0;
    L := Max(Abs(X1 - X2), Abs(Y1 - Y2)) + 1;
    for I := 1 to L do
    begin
      R := I / L;
      AX := X1 + Trunc((X2 - X1) * R);
      AY := Y1 + Trunc((Y2 - Y1) * R);
      if (B = 0) and (Map[AX, AY] = TILE_WALL) then
      begin
        Map[AX, AY] := TileDoor();
        Inc(B);
        Continue;
      end;
      if (B = 1) and (Map[AX, AY] = TILE_FLOOR) then
      begin
        Map[LX, LY] := TileDoor();
        Inc(B);
        Continue;
      end;
      Map[AX, AY] := TILE_FLOOR;
      LX := AX;
      LY := AY;
    end;
  end;

begin
  MapClear(TILE_WALL);
  RoomsCount := (AX div 10) * (AY div 10);
  I := 0; J := 0; L.X := 0; L.Y := 0;
  SetLength(V, I + 1);
  while (J <= RoomsCount - 1) do
  begin
    P.X := Rand(2, 7);
    P.Y := Rand(2, 7);
    V[I].X := Rand(2 + P.X, AX - P.X - 3);
    V[I].Y := Rand(2 + P.Y, AY - P.Y - 3);
    if FillRegion(V[I].X, V[I].Y, P.X, P.X, TILE_FLOOR) then
    begin
      if (L.X > 0) and (L.Y > 0) then LinkRoom(V[I].X, V[I].Y, L.X, L.Y);
      L.X := V[I].X;
      L.Y := V[I].Y;
      Inc(I);
      SetLength(V, I + 1);
    end;
    Inc(J);
  end;
end;

// #17
procedure DoomRoomsCreate(AX, AY: Integer);
var
  I, L, C: Integer;
  V: array of TRect;
  X1, Y1, W1, H1, X2, Y2, W2, H2: Integer;

  procedure AddTile(X, Y: Integer);
  begin
    if (Map[X, Y] = TILE_FLOOR) then Exit;
    if (Map[X, Y] = TILE_WALL) then
      Map[X, Y] := TILE_FLOOR;
  end;

  procedure ClearArts();
  var
    X, Y: Integer;
  begin
    for Y := 1 to AY - 1 do
      for X := 1 to AX - 1 do
      begin
        if (Map[X, Y] = TILE_DOOR) or (Map[X, Y] = TILE_OPEN_DOOR) then
          if (((Map[X - 1, Y] = TILE_WALL) and (Map[X + 1, Y] = TILE_WALL)
          and (Map[X, Y - 1] = TILE_FLOOR) and (Map[X, Y + 1] = TILE_FLOOR))
          or ((Map[X, Y - 1] = TILE_WALL) and (Map[X, Y + 1] = TILE_WALL))
          and (Map[X - 1, Y] = TILE_FLOOR) and (Map[X + 1, Y] = TILE_FLOOR))
          then Continue else Map[X, Y] := TILE_FLOOR;
      end;
    for Y := 1 to AY - 1 do
      for X := 1 to AX - 1 do
        if (Map[X, Y] = TILE_FLOOR) then
        begin
          if ((Map[X - 1, Y] = TILE_WALL) and (Map[X + 1, Y] = TILE_WALL)
            and (Map[X, Y + 1] = TILE_WALL)) then Map[X, Y] := TILE_WALL;
          if ((Map[X - 1, Y] = TILE_WALL) and (Map[X + 1, Y] = TILE_WALL)
            and (Map[X, Y - 1] = TILE_WALL)) then Map[X, Y] := TILE_WALL;
          if ((Map[X, Y - 1] = TILE_WALL) and (Map[X, Y + 1] = TILE_WALL)
            and (Map[X + 1, Y] = TILE_WALL)) then Map[X, Y] := TILE_WALL;
          if ((Map[X, Y - 1] = TILE_WALL) and (Map[X, Y + 1] = TILE_WALL)
            and (Map[X - 1, Y] = TILE_WALL)) then Map[X, Y] := TILE_WALL;
        end;
  end;

  function AddRooms(X1, Y1, W1, H1, TILE_FLOOR: Integer): Boolean;
  begin
    Result := FillRegion(X1, Y1, W1, H1, TILE_FLOOR);
    if Result and (Rand(1, 1) = 1) and (W1 > 4) and (H1 > 4) then
    begin
      SetLength(V, L + 1);
      V[L].X := X1;
      V[L].Y := Y1;
      V[L].W := W1;
      V[L].H := H1;
      Inc(L);
    end;
  end;

  procedure AddInRoom(I: Integer);
  begin
    case Rand(1, 2) of
//      1: CreateSomething(4, 20, False);
      2: DrawRoom(V[I].X - Rand(2, 4), V[I].Y - Rand(2, 4),
           V[I].X + Rand(2, 4), V[I].Y + Rand(2, 4));
    end;
  end;

  procedure LinkRooms(X1, Y1, W1, H1, X2, Y2, W2, H2: Integer);
  var
    X, Y: Integer;
    A, B: Boolean;
  begin
    A := False;
    B := False;
    case Rand(0, 2) of
      1: A := True;
      2: B := True;
      else begin
        A := True;
        B := True;
      end;
    end;
    if A then
    begin
      {v. 1}
      if (X1 < X2) then begin
        for X := X1 to X2 - W2 - 2 do AddTile(X, Y2);
        Map[X2 - W2 - 1, Y2] := TileDoor();
      end else begin
        for X := X2 + W2 + 2 to X1 do AddTile(X, Y2);
        Map[X2 + W2 + 1, Y2] := TileDoor();
      end;
      if (Y1 < Y2) then begin
        for Y := Y1 + H1 + 2 to Y2 do AddTile(X1, Y);
        Map[X1, Y1 + H1 + 1] := TileDoor();
      end else begin
        for Y := Y2 to Y1 - H1 - 2 do AddTile(X1, Y);
        Map[X1, Y1 - H1 - 1] := TileDoor();
      end;
    end;
    if B then begin
      {v. 2}
      if (X1 < X2) then begin
        for X := X1 + W1 + 2 to X2 do AddTile(X, Y1);
        Map[X1 + W1 + 1, Y1] := TileDoor();
      end else begin
        for X := X2 to X1 - W1 - 2 do AddTile(X, Y1);
        Map[X1 - W1 - 1, Y1] := TileDoor();
      end;
      if (Y1 < Y2) then begin
        for Y := Y1 to Y2 - H2 - 2 do AddTile(X2, Y);
        Map[X2, Y2 - H2 - 1] := TileDoor();
      end else begin
        for Y := Y2 + H2 + 2 to Y1 do AddTile(X2, Y);
        Map[X2, Y2 + H2 + 1] := TileDoor();
      end;
    end;
  end;

begin
  L := 0;
  W1 := Rand(2, 9);
  H1 := Rand(2, 9);
  X1 := Rand(20, AX - 20);
  Y1 := Rand(20, AY - 20);
  MapClear(TILE_WALL);
  AddRooms(X1, Y1, W1, H1, TILE_FLOOR);
  C := (AX div 2) * (AY div 2);
  for I := 0 to C do
  begin
    W2 := Rand(2, 10);
    H2 := Rand(2, 10);
    X2 := Rand(W2 * 2 + 1, AX - (W2 * 2 + 1));
    Y2 := Rand(H2 * 2 + 1, AY - (H2 * 2 + 1));
    if (X1 = X2) then Inc(X2, Rand(2, 3));
    if (Y1 = Y2) then Inc(Y2, Rand(2, 3));
    if AddRooms(X2, Y2, W2, H2, TILE_FLOOR) then
    begin   
      LinkRooms(X1, Y1, W1, H1, X2, Y2, W2, H2);
      X1 := X2;
      Y1 := Y2;
      W1 := W2;
      H1 := H2;
    end;
  end;
  if (L > 0) then for I := 0 to L - 1 do AddInRoom(I);

{  //
  for Y := 1 to AY - 1 do
    for X := 1 to AX - 1 do
      if (CountNearby(X, Y, TILE_WALL) <= 3) then
        Map[X, Y] := TILE_FLOOR;}

  ClearArts();
end;

// #18
procedure DarkCaveCreate(AX, AY: Integer; Flag: Boolean);
type
  CellState = (csEdge, csChaos, csIn, csOut, csFloor);
  StateMapArray = array[-100..MAXX + 100, -100..MAXY + 100] of CellState;

var
  StateMap: StateMapArray;
  A, B, X, Y, I, J: Integer;
  MinX, MaxX, MinY, MaxY: Integer;

  procedure ClearStateMap;
  var
    I, J: Integer;
  begin
    for I := -100 to MAXX + 100 do
      for J := -100 to MAXX + 100 do
      begin
        if ((I <= 1) or (I >= AX)) or ((J <= 1) or (J >= AY)) then
        StateMap[I, J] := csEdge else StateMap[I, J] := csChaos;
      end;
  end;

  procedure Make(var X, Y: Integer; Flag: Boolean);
  var
    I, DX, DY: Integer;
  begin
    for I := 0 to 9999 do
    begin
        DX := Rand(-1, 1);
        DY := Rand(-1, 1);
        if (MinX > 2) and (Rand(1, 3) = 1) then DX := -1;
        if (MaxX < AX - 1) and (Rand(1, 3) = 1) then DX := 1;
        if (MinY > 2) and (Rand(1, 3) = 1) then DY := -1;
        if (MaxY < AY - 1) and (Rand(1, 3) = 1) then DY := 1;
      if (StateMap[DX + X, DY + Y] = csChaos) then
      begin
        X := DX + X;
        Y := DY + Y;
        MinX := Min(X, MinX);
        MaxX := Max(X, MaxX);
        MinY := Min(Y, MinY);
        MaxY := Max(Y, MaxY);
        StateMap[X, Y] := csFloor;
        if Flag and (StateMap[X + 1, Y] = csChaos) then StateMap[X + 1, Y] := csFloor;
        if Flag and (StateMap[X, Y + 1] = csChaos) then StateMap[X, Y + 1] := csFloor;
      end;
    end;
  end;

begin
  repeat
    ClearStateMap;
    MinX := AX;
    MinY := AY;
    MaxX := 0;
    MaxY := 0;
    A := Rand(2, AX - 1);
    B := Rand(2, AY - 1);
    StateMap[A, B] := csIn;
    X := A; Y := B;
    Make(X, Y, Flag);
  until((MinX = 2) and (MinY = 2) and (MaxX = AX - 1) and (MaxY = AY - 1) and (A <> X) and (B <> Y));
  StateMap[X, Y] := csOut;
  // Создаем тайловую карту из StateMap
  MapClear(TILE_WALL);
  for I := -100 to MAXX + 100 do
    for J := -100 to MAXX + 100 do
    case StateMap[I, J] of
      csEdge, csChaos: Map[I, J] := TILE_WALL;
      csIn: Map[I, J] := TILE_IN;
      csOut: Map[I, J] := TILE_OUT;
      csFloor: Map[I, J] := TILE_FLOOR;
    end;
end;

// #20
procedure RedRoomsCreate(AX, AY: Integer);
var
  I: Integer;

  function InsRoom: Boolean;
  var
    X, Y, W, H: Integer;
  begin
    X := Rand(1, AX - 1);
    Y := Rand(1, AY - 1);
    W := Rand(1, 3);
    H := Rand(1, 3);
    Result := FillRegion(X, Y, W, H, TILE_FLOOR);
  end;
  
begin
  MapClear(TILE_WALL);
  for I := 0 to 20 do InsRoom;
end;

// #21
procedure DarkForestCreate(X, Y: Integer);
begin
  MapClear(TILE_FLOOR);
  ForestCreate(X, Y);
end;

// #22
procedure StonyFieldCreate(X, Y: Integer);      
var
  I, J: Integer;
begin
  MapClear(TILE_FLOOR);
  TundraCreate(X,Y);
  for i:=1 to X do
    for j:=1 to Y do
      if (map[i,j]=TILE_TREE) then
      begin
        if (Rand(1, 5) = 1) then
          map[i,j]:=GetTree
        else
          map[i,j]:=TILE_STONE;
      end else
        map[i,j]:=TILE_FLOOR;
end;

procedure CreateMap(X, Y, ID: Integer; var A: TBeaRLibMap; S: Integer);
var
  I, J: Integer;
begin
  if ((ID <= 0) or (ID > MaxID) or (X < MinX) or (Y < MinY)
    or (X > MaxX) or (Y > MaxY)) then Exit;
  MapClear(TILE_FLOOR);
  MapX := X;
  MapY := Y;
  Randomize;
  case ID of
    G_ANT_NEST:       AntNestCreate(1,1,X,Y,0);
    G_CAVES:          CavesCreate(X,Y);
    G_LAKES:          LakesCreate(1,1,X,Y,0);
    G_LAKES2:         LakesCreate(1,1,X,Y,1);
    G_TOWER:          TowerCreate(X,Y);
    G_HIVE:           AntNestCreate(1,1,X,Y,1);
    G_CITY:           CityCreate(X,Y,0);
    G_MOUNTAIN:       AntNestCreate(1,1,X,Y,2);
    G_FOREST:         ForestCreate(X,Y);
    G_VILLAGE:        VillageCreate(X,Y,0);
    G_SWAMP:          SwampCreate(X,Y);
    G_RIFT:           RiftCreate(X,Y);
    G_TUNDRA:         TundraCreate(X,Y);
    G_BROKEN_CITY:    CityCreate(X,Y,1);
    G_BROKEN_VILLAGE: VillageCreate(X, Y, 1);
    G_DARK_ROOMS:     DarkRoomsCreate(X, Y);
    G_DOOM_ROOMS:     DoomRoomsCreate(X, Y);
    G_DARK_CAVE:      DarkCaveCreate(X, Y, False);
    G_DARK_GROTTO:    DarkCaveCreate(X, Y, True);
    G_RED_ROOMS:      RedRoomsCreate(X, Y);
    G_DARK_FOREST:    DarkForestCreate(X, Y);
    G_STONY_FIELD:    StonyFieldCreate(X, Y);
  end; // case

  try
    case ID of
      G_ANT_NEST, G_CAVES, G_HIVE, G_DARK_ROOMS, G_DOOM_ROOMS:
      begin
        for I := 1 to X do
        begin
          Map[I, 1] := TILE_WALL;
          Map[I, Y] := TILE_WALL;
        end;
        for I := 1 to Y do
        begin
          Map[1, I] := TILE_WALL;
          Map[X, I] := TILE_WALL;
        end;
      end;
      G_FOREST, G_DARK_FOREST:
      begin
        for I := 1 to X do
        begin
          Map[I, 1] := GetTree();
          Map[I, Y] := GetTree();
        end;
        for I := 1 to Y do
        begin
          Map[1, I] := GetTree();
          Map[X, I] := GetTree();
        end;
      end;
    end;
    for I := 0 to X - 1 do
      for J := 0 to Y - 1 do
        A[I * Y + J] := Tileset[Map[I+1][J+1]].CH;
   except end;
end;


end.
