unit Trollhunter.Graph;

interface

uses
  Graphics,
  Trollhunter.Color;

type
  TMessagebar = class(TObject)
  private
    FText: string;
    procedure SetText(const Value: string);
  public
    property Text: string read FText write SetText;
    procedure Add(Msg: string);
    procedure Clear;
    procedure Render;
    constructor Create;
    destructor Destroy; override;
  end;

  TText = class(TObject)
  private
    FKeyCursorPos: Integer;
  public
    procedure BarOut(const KeyID, StrID: string;
      IsNewBar: Boolean = False); overload;
    procedure BarOut(const KeyID: string; LID: Word;
      IsNewBar: Boolean = False); overload;
    function ClearText(const S: string): string;
    procedure DrawText(const X, Y: Integer; const S: string);
    procedure DrawOut(const X, Y: Integer; const S: string);
    procedure TitleOut(const S: string; P: Integer = 0);
    procedure TextCenter(const PY: Integer; const StrID: string);
    constructor Create;
    destructor Destroy; override;
  end;

  TBars = class(TObject)
  private
    LIFEBAR: Graphics.TBitmap;
    MANABAR: Graphics.TBitmap;
    EXPBAR: Graphics.TBitmap;
  public
    EXP: Graphics.TBitmap;
    LIFE, PLIFE: Graphics.TBitmap;
    MANA: Graphics.TBitmap;
    DAMAGE: Graphics.TBitmap;
    PROTECT: Graphics.TBitmap;
    procedure Render;
    constructor Create;
    destructor Destroy; override;
  end;

  TGraph = class
  private
    FSurface: Graphics.TBitmap;
    FCanvas: TCanvas;
    FCharWidth: Integer;
    FCharHeight: Integer;
    FMessagebar: TMessagebar;
    FText: TText;
    FBars: TBars;
    FDownLine: Integer;
    FPW: Integer;
    FRW: Integer;
    FDL: Integer;
    FRH: Integer;
    procedure SetSurface(const Value: Graphics.TBitmap);
    procedure SetCanvas(const Value: TCanvas);
    procedure SetCharHeight(const Value: Integer);
    procedure SetCharWidth(const Value: Integer);
    procedure SetMessagebar(const Value: TMessagebar);
    procedure SetText(const Value: TText);
    procedure SetBars(const Value: TBars);
    procedure SetDownLine(const Value: Integer);
    procedure SetDL(const Value: Integer);
    procedure SetPW(const Value: Integer);
    procedure SetRH(const Value: Integer);
    procedure SetRW(const Value: Integer);
  public
    property RW: Integer read FRW write SetRW;
    property RH: Integer read FRH write SetRH;
    property DL: Integer read FDL write SetDL;
    property PW: Integer read FPW write SetPW;
    property DownLine: Integer read FDownLine write SetDownLine;
    property CharWidth: Integer read FCharWidth write SetCharWidth;
    property CharHeight: Integer read FCharHeight write SetCharHeight;
    procedure Clear(AColor: Integer);
    procedure Render();
    procedure BitmapFromTileset(Tile, Tileset: Graphics.TBitmap;
      Index: Integer);
    procedure ImageFromTileset(Bitmap: Graphics.TBitmap; Index: Integer);
    procedure ModTileColor(var BaseTile: Graphics.TBitmap; ResTileName: string;
      AColor: Integer);
    function Width: Integer;
    function Height: Integer;
    procedure RenderMenu(P, T: Integer; C: Integer = cMnColor);
    procedure Default;
    property Bars: TBars read FBars write SetBars;
    property Text: TText read FText write SetText;
    property Surface: Graphics.TBitmap read FSurface write SetSurface;
    property Canvas: TCanvas read FCanvas write SetCanvas;
    property Messagebar: TMessagebar read FMessagebar write SetMessagebar;
    constructor Create(AWidth, AHeight, AFontSize: Integer; ACanvas: TCanvas);
    destructor Destroy; override;
  end;

  TJPEGBitmap = class(Graphics.TBitmap)
  public
    constructor Create(); override;
    destructor Destroy(); override;
  public
    procedure LoadFromFile(const FileName: String); override;
  end;

var
  Graph: TGraph;

implementation

uses
  Classes,
  SysUtils,
  Types,
  Windows,
  Forms,
  JPEG,
  Trollhunter.Creatures,
  Trollhunter.Map,
  Trollhunter.Log,
  Trollhunter.MainForm,
  Trollhunter.Utils,
  Trollhunter.Error,
  Trollhunter.Settings,
  Trollhunter.Lang;

{$R images.res}
{ TGraph }

{
  procedure TForm1.flip(var b: tbitmap ;ver,hor : boolean);
  begin
  if hor
  then stretchblt(b.canvas.handle,0,Gsize-1,Gsize,-Gsize,b.canvas.handle,0,0,Gsize,Gsize,srccopy);
  if ver
  then stretchblt(b.canvas.handle,Gsize-1,0,-Gsize,Gsize,b.canvas.handle,0,0,Gsize,Gsize,srccopy);
  end;
}

procedure TGraph.Clear(AColor: Integer);
begin
  Surface.Canvas.Brush.Color := AColor;
  Surface.Canvas.FillRect(Rect(0, 0, Surface.Width, Surface.Height));
end;

procedure TGraph.Default;
const
  PanelWidth = 300;
begin
  CharWidth := Surface.Canvas.TextWidth('@');
  CharHeight := Surface.Canvas.TextHeight('@');
  DownLine := ((Surface.Height div CharHeight) * CharHeight) - CharHeight;
  RW := (((Surface.Width - PanelWidth) div TileSize) div 2);
  RH := ((Surface.Height div TileSize) div 2);
  DL := (RW * 2) * TileSize + TileSize;
  PW := Surface.Width - DL;
  with MainForm do
  begin
    ClientWidth := Surface.Width;
    ClientHeight := Surface.Height;
    if (Screen.Width = Surface.Width) and (Screen.Height = Surface.Height) then
      BorderStyle := bsNone
    else
      BorderStyle := bsSizeable;
  end;
  Bars.Free;
  Bars := TBars.Create;
  ScaleBmp(Bars.EXPBAR, PW, CharHeight - 1);
  ScaleBmp(Bars.LIFEBAR, PW, CharHeight - 1);
  ScaleBmp(Bars.MANABAR, PW, CharHeight - 1);
  ScaleBmp(Bars.PROTECT, CharHeight - 1, CharHeight - 1);
  ScaleBmp(Bars.DAMAGE, CharHeight - 1, CharHeight - 1);
  ScaleBmp(Bars.LIFE, CharHeight - 1, CharHeight - 1);
  ScaleBmp(Bars.PLIFE, CharHeight - 1, CharHeight - 1);
  ScaleBmp(Bars.MANA, CharHeight - 1, CharHeight - 1);
  ScaleBmp(Bars.EXP, CharHeight - 1, CharHeight - 1);
end;

constructor TGraph.Create(AWidth, AHeight, AFontSize: Integer;
  ACanvas: TCanvas);
var
  S: TSettings;
begin
  Messagebar := TMessagebar.Create;
  Text := TText.Create;
  Canvas := ACanvas;
  Surface := Graphics.TBitmap.Create();
  Surface.Canvas.Brush.Style := bsClear;
  Surface.Canvas.Font.Name := 'Courier New';
  S := TSettings.Create;
  try
    LangID := S.Read('Settings', 'Language', 0);
    Surface.Canvas.Font.Size := S.Read('Settings', 'FontSize',
      Clamp(AFontSize, 10, 20));
    TileSize := S.Read('Settings', 'TileSize', 0) * 16 + BaseTileSize;
  finally
    S.Free;
  end;
  Surface.Canvas.Font.Style := [];
  Surface.Width := AWidth;
  Surface.Height := AHeight;
  Self.Default;
end;

destructor TGraph.Destroy;
begin
  Surface.Free;
  Text.Free;
  Messagebar.Free;
  Bars.Free;
  inherited;
end;

procedure TGraph.Render();
begin
  if (MainForm.BorderStyle <> bsNone) then
    Canvas.StretchDraw(Rect(0, 0, MainForm.ClientWidth,
      MainForm.ClientHeight), Surface)
  else
    Canvas.Draw(0, 0, Surface);
end;

procedure TGraph.BitmapFromTileset(Tile, Tileset: Graphics.TBitmap;
  Index: Integer);
var
  Col, Row, ColCount, RowCount: Integer;
begin
  if (Index < 0) then
    Index := 0;
  ColCount := Tileset.Width div BaseTileSize;
  RowCount := Tileset.Height div BaseTileSize;
  if (Index > (ColCount * RowCount) - 1) then
    Index := (ColCount * RowCount) - 1;
  Tile.Width := TileSize;
  Tile.Height := TileSize;
  Tile.PixelFormat := pf24bit;
  if (ColCount > 0) then
  begin
    Col := Index mod ColCount;
    Row := Index div ColCount;
  end
  else
  begin
    Col := 0;
    Row := 0;
  end;
  Tile.Canvas.CopyRect(Bounds(0, 0, TileSize, TileSize), Tileset.Canvas,
    Bounds(Col * BaseTileSize, Row * BaseTileSize, BaseTileSize, BaseTileSize));
end;

procedure TGraph.ImageFromTileset;
var
  TempBitmap: Graphics.TBitmap;
  I, J, X, Y, Z: Integer;
begin
  TempBitmap := Graphics.TBitmap.Create;
  TempBitmap.Width := TileSize;
  TempBitmap.Height := TileSize;
  try
    Z := 0;
    if (Index < 0) then
      Index := 0;
    for J := 0 to (Bitmap.Height div TileSize) - 1 do
    begin
      for I := 0 to (Bitmap.Width div TileSize) - 1 do
      begin
        if (Z = Index) then
        begin
          X := TileSize * I;
          Y := TileSize * J;
          TempBitmap.Height := TileSize;
          TempBitmap.Width := TileSize;
          TempBitmap.Canvas.CopyRect(Bounds(0, 0, TileSize, TileSize),
            Bitmap.Canvas, Bounds(X, Y, TileSize, TileSize));
          Exit;
        end;
        Inc(Z);
      end;
    end;
  finally
    Bitmap.Assign(TempBitmap);
    TempBitmap.Free;
  end;
end;

function TGraph.Height: Integer;
begin
  Result := Surface.Height
end;

function TGraph.Width: Integer;
begin
  Result := Surface.Width
end;

procedure TGraph.SetSurface(const Value: Graphics.TBitmap);
begin
  FSurface := Value
end;

procedure TGraph.SetCanvas(const Value: TCanvas);
begin
  FCanvas := Value
end;

procedure TGraph.SetCharHeight(const Value: Integer);
begin
  FCharHeight := Value
end;

procedure TGraph.SetCharWidth(const Value: Integer);
begin
  FCharWidth := Value
end;

procedure TGraph.ModTileColor(var BaseTile: Graphics.TBitmap;
  ResTileName: string; AColor: Integer);
var
  T: Graphics.TBitmap;
begin
  T := Graphics.TBitmap.Create;
  BaseTile.Width := TileSize;
  BaseTile.Height := TileSize;
  BaseTile.Canvas.Brush.Color := AColor;
  BaseTile.Canvas.FillRect(Rect(0, 0, TileSize, TileSize));
  T.Handle := LoadBitmap(hInstance, PChar(ResTileName));
  ScaleBmp(T, TileSize, TileSize);
  T.TransparentColor := T.Canvas.Pixels[TileSize div 2, Round(TileSize / 2.6)];
  T.Transparent := True;
  BaseTile.Canvas.Draw(0, 0, T);
  T.Free;
end;

procedure TGraph.RenderMenu(P, T, C: Integer);
begin
  with Surface.Canvas do
  begin
    Brush.Color := C;
    FillRect(Rect(CharWidth, P * CharHeight + T, Surface.Width - CharWidth,
      (P + 1) * CharHeight + T));
    Brush.Color := 0;
    Brush.Style := bsClear;
  end;
end;

{ TMessagebar }

procedure TMessagebar.Add(Msg: string);
begin
  if (Text <> Msg) then
    Text := Trim(Text + ' ' + Msg)
end;

procedure TMessagebar.Clear;
begin
  Text := ''
end;

constructor TMessagebar.Create;
begin
  Clear
end;

destructor TMessagebar.Destroy;
begin

  inherited;
end;

procedure TMessagebar.Render;
var
  N: string;
  X, I, L, V: Integer;
  C: Char;
  T: array [0 .. 9] of Integer;
  B, D: Boolean;
begin
  try
    with Graph.Surface.Canvas do
    begin
      Font.Style := [];
      Brush.Color := 0;
      T[0] := cWhiteYel;
      Font.Color := cWhiteYel;
      FillRect(Rect(0, 0, Graph.Surface.Width, Graph.CharHeight));
      if (Text <> '') then
      begin
        B := False;
        D := False;
        X := 0;
        V := 0;
        I := 1;
        L := Length(Text);
        while (I <= L) do
        begin
          I := Clamp(I, 1, L);
          C := Text[I];
          if (C = '#') then
          begin
            Inc(I);
            B := True;
            Continue;
          end;
          if B then
          begin
            B := False;
            Inc(V);
            T[V] := Font.Color;
            Font.Style := [fsBold];
            Font.Color := CharToColor(Text[I]);
            Inc(I);
            Continue;
          end;
          if (C = '$') then
          begin
            Inc(I);
            D := True;
            Continue;
          end;
          if D then
          begin
            D := False;
            if (V > 0) then
              Dec(V);
            Font.Color := T[V];
            Font.Style := [];
          end;
          TextOut(X * Graph.CharWidth, 0, C);
          Inc(X);
          Inc(I);
        end;
      end;
      Font.Color := T[0];
      Font.Style := [];
      N := GetMapLang(Map.Info.ID);
      if (N <> '') then
        TextOut(Graph.Width - TextWidth(N), Graph.CharHeight, N);
    end;
  except
    on E: Exception do
      Error.Add('Messagebar.Render', E.Message);
  end;
end;

procedure TMessagebar.SetText(const Value: string);
begin
  FText := Value;
end;

{ TText }

procedure TText.BarOut(const KeyID, StrID: string; IsNewBar: Boolean);
var
  K: string;
  C: Integer;
begin
  try
    K := '[' + KeyID + ']';
    if IsNewBar then
      FKeyCursorPos := 0;
    with Graph.Surface.Canvas do
    begin
      C := Font.Color;
      Font.Style := [fsBold];
      Font.Color := cLtYellow;
      TextOut(FKeyCursorPos * Graph.CharWidth, Graph.DownLine, K);
      Inc(FKeyCursorPos, Length(K) + 1);
      Font.Color := cRdYellow;
      TextOut(FKeyCursorPos * Graph.CharWidth, Graph.DownLine,
        AnsiLowerCase(StrID));
      Inc(FKeyCursorPos, Length(StrID) + 1);
      Font.Style := [];
      Font.Color := C;
    end;
  except
    on E: Exception do
      Error.Add('Text.BarOut', E.Message);
  end;
end;

procedure TText.BarOut(const KeyID: string; LID: Word; IsNewBar: Boolean);
begin
  Self.BarOut(KeyID, GetLang(LID), IsNewBar);
end;

function TText.ClearText(const S: string): string;
var
  I, N: Integer;
  C: Char;
begin
  N := 0;
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S.Chars[N];
    case C of
      '#':
        begin
          Inc(N, 2);
          Continue;
        end;
      '$':
        begin
          Inc(N);
          Continue;
        end;
    end;
    Result := Result + C;
    Inc(N);
  end;
  Result := Trim(Result);
end;

constructor TText.Create;
begin
  FKeyCursorPos := 0;
end;

destructor TText.Destroy;
begin

  inherited;
end;

procedure TText.DrawOut(const X, Y: Integer; const S: string);
begin
  Graph.Surface.Canvas.TextOut(X * Graph.CharWidth, Y * Graph.CharHeight, S);
end;

procedure TText.DrawText(const X, Y: Integer; const S: string);
var
  T: string;
  V: Char;
  F: Boolean;
  I, C, K, N: Integer;
begin
  F := False;
  if S.Contains('#') then
    with Graph.Surface.Canvas do
    begin
      K := 0;
      N := 0;
      C := Font.Color;
      for I := 1 to Length(S) do
      begin
        V := S.Chars[K];
        case V of
          '#':
            begin
              Inc(K, 2);
              Font.Color := CharToColor(S.Chars[K - 1]);
              Continue;
            end;
          '$':
            begin
              Inc(K);
              Font.Color := C;
              Continue;
            end;
        end;
        TextOut((X + N) * Graph.CharWidth, Y * Graph.CharHeight, V);
        Inc(K);
        Inc(N);
      end;
      Font.Color := C;
    end;
end;

procedure TText.TextCenter(const PY: Integer; const StrID: string);
begin
  try
    with Graph.Surface.Canvas do
      TextOut((Graph.Width div 2) - (TextWidth(StrID) div 2),
        PY * Graph.CharHeight, StrID);
  except
    on E: Exception do
      Error.Add('Text.TextCenter', E.Message);
  end;
end;

procedure TText.TitleOut(const S: string; P: Integer = 0);
var
  I, H, C: Integer;
  L, T: string;
begin
  try
    T := ClearText(S);
    for I := 1 to Length(T) do
      L := L + '=';
    with Graph.Surface.Canvas do
    begin
      C := Font.Color;
      Font.Color := cRdYellow;
      Font.Style := [fsBold];
      H := (Graph.Width div 2) - (TextWidth(T) div 2);
      TextOut(H, P * TextHeight(T), T);
      TextOut(H, (P + 1) * TextHeight(T), L);
      Font.Style := [];
      Font.Color := C;
    end;
  except
    on E: Exception do
      Error.Add('Text.TitleOut', E.Message);
  end;
end;

{ TBars }

constructor TBars.Create;
begin
  EXP := Graphics.TBitmap.Create;
  EXP.Handle := LoadBitmap(hInstance, 'EXP');
  LIFE := Graphics.TBitmap.Create;
  LIFE.Handle := LoadBitmap(hInstance, 'LIFE');
  PLIFE := Graphics.TBitmap.Create;
  PLIFE.Handle := LoadBitmap(hInstance, 'PLIFE');
  MANA := Graphics.TBitmap.Create;
  MANA.Handle := LoadBitmap(hInstance, 'MANA');

  EXPBAR := Graphics.TBitmap.Create;
  EXPBAR.Handle := LoadBitmap(hInstance, 'PCEXPBAR');
  LIFEBAR := Graphics.TBitmap.Create;
  LIFEBAR.Handle := LoadBitmap(hInstance, 'PCLIFEBAR');
  MANABAR := Graphics.TBitmap.Create;
  MANABAR.Handle := LoadBitmap(hInstance, 'PCMANABAR');
  DAMAGE := Graphics.TBitmap.Create;
  DAMAGE.Handle := LoadBitmap(hInstance, 'DAMAGE');
  PROTECT := Graphics.TBitmap.Create;
  PROTECT.Handle := LoadBitmap(hInstance, 'PROTECT');
end;

destructor TBars.Destroy;
begin
  EXPBAR.Free;
  LIFEBAR.Free;
  MANABAR.Free;
  PROTECT.Free;
  DAMAGE.Free;
  LIFE.Free;
  PLIFE.Free;
  MANA.Free;
  EXP.Free;
  inherited;
end;

procedure TBars.Render;
const
  C = '%d/%d';
var
  I: Byte;
  M, R, W, Left, Top: Integer;
  T: Graphics.TBitmap;
  S: array [1 .. 3] of string;

  procedure DrawBar(Top: Byte);
  begin
    Graph.Surface.Canvas.Draw(Graph.Width - T.Width,
      (Graph.CharHeight * Top), T);
  end;

begin
  T := Graphics.TBitmap.Create;
  try
    Left := Graph.DL + (Graph.CharWidth * 2) + 4;
    S[1] := Format(C, [Creatures.PC.LIFE.Cur, Creatures.PC.LIFE.Max]);
    S[2] := Format(C, [Creatures.PC.MANA.Cur, Creatures.PC.MANA.Max]);
    S[3] := Format(C, [Creatures.PC.Prop.EXP, Creatures.PC.MaxExp]);
    M := 0;
    for I := 1 to 3 do
      if (Length(S[I]) > M) then
        M := Length(S[I]);
    W := Graph.PW - (Graph.CharWidth * (M + 3));
    with Graph.Surface.Canvas do
    begin
      Brush.Color := 0;
      // Life
      if Creatures.PC.TempSys.IsVar('Poison') then
      begin
        Draw(Graph.DL + 4, (Graph.CharHeight * 2), PLIFE);
        Font.Color := cFxGreen;
      end
      else
      begin
        Draw(Graph.DL + 4, (Graph.CharHeight * 2), LIFE);
        Font.Color := cRdRed;
      end;
      TextOut(Left, (Graph.CharHeight * 2), S[1]);
      if not Creatures.PC.LIFE.IsMin then
      begin
        T.Assign(LIFEBAR);
        if (T.Width > W) then
          T.Width := W;
        T.Width := BarWidth(Creatures.PC.LIFE.Cur,
          Creatures.PC.LIFE.Max, T.Width);
        DrawBar(2);
      end;
      // Mana
      Font.Color := cRdBlue;
      Draw(Graph.DL + 4, (Graph.CharHeight * 3), MANA);
      TextOut(Left, (Graph.CharHeight * 3), S[2]);
      if not Creatures.PC.MANA.IsMin then
      begin
        T.Assign(MANABAR);
        if (T.Width > W) then
          T.Width := W;
        T.Width := BarWidth(Creatures.PC.MANA.Cur,
          Creatures.PC.MANA.Max, T.Width);
        DrawBar(3);
      end;
      // Exp
      Font.Color := cRdBrown;
      Draw(Graph.DL + 4, (Graph.CharHeight * 4), EXP);
      TextOut(Left, (Graph.CharHeight * 4), S[3]);
      if (Creatures.PC.Prop.EXP > 0) then
      begin
        if (Creatures.PC.Prop.Level > 1) then
          R := Creatures.PC.MaxExp(Creatures.PC.Prop.Level - 1)
        else
          R := 0;
        T.Assign(EXPBAR);
        if (T.Width > W) then
          T.Width := W;
        T.Width := BarWidth(Creatures.PC.Prop.EXP - R,
          Creatures.PC.MaxExp - R, T.Width);
        DrawBar(4);
      end;
      // Damage, Protect
      Font.Color := cRdPurple;
      Top := Graph.CharHeight * 5;
      S[1] := Format('%d-%d', [Creatures.PC.Prop.MinDamage,
        Creatures.PC.Prop.MaxDamage]);
      S[2] := IntToStr(Creatures.PC.Prop.PROTECT);
      M := Graph.DL + 4;
      Draw(M, Top, DAMAGE);
      M := M + (Graph.CharWidth * 2);
      TextOut(M, Top, S[1]);
      M := M + (Graph.CharWidth * Length(S[1]));
      Draw(M, Top, PROTECT);
      M := M + (Graph.CharWidth * 2);
      TextOut(M, Top, S[2]);
    end;
  except
    on E: Exception do
      Error.Add('Bars.Render', E.Message);
  end;
  T.Free;
end;

procedure TGraph.SetMessagebar(const Value: TMessagebar);
begin
  FMessagebar := Value;
end;

procedure TGraph.SetText(const Value: TText);
begin
  FText := Value;
end;

procedure TGraph.SetBars(const Value: TBars);
begin
  FBars := Value;
end;

procedure TGraph.SetDownLine(const Value: Integer);
begin
  FDownLine := Value;
end;

procedure TGraph.SetDL(const Value: Integer);
begin
  FDL := Value;
end;

procedure TGraph.SetPW(const Value: Integer);
begin
  FPW := Value;
end;

procedure TGraph.SetRH(const Value: Integer);
begin
  FRH := Value;
end;

procedure TGraph.SetRW(const Value: Integer);
begin
  FRW := Value;
end;

{ TJPEGBitmap }

constructor TJPEGBitmap.Create;
begin
  inherited;

end;

destructor TJPEGBitmap.Destroy;
begin

  inherited;
end;

procedure TJPEGBitmap.LoadFromFile(const FileName: string);
var
  vFileExt: string;
  vJPEG: TJPEGImage;
begin
  if not FileExists(FileName) then
    Exit;
  vFileExt := LowerCase(ExtractFileExt(FileName));
  if (vFileExt = '.jpg') or (vFileExt = '.jpeg') then
  begin
    vJPEG := TJPEGImage.Create();
    vJPEG.LoadFromFile(FileName);
    Assign(vJPEG);
    vJPEG.Free();
  end;
  if (vFileExt = '.bmp') then
    inherited LoadFromFile(FileName);
end;

end.
