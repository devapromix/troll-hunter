unit Trollhunter.Effect;

interface

uses
  Graphics;

type
  TEffectEnum = (efBlind, efPoison, efLife, efMana, efWizardEye, efWebbed);

const
  EffectStr: array [TEffectEnum] of string = ('Blind', 'Poison', 'VialOfLife',
    'VialOfMana', 'WizardEye', 'Webbed');

type
  TEffects = class(TObject)
  private
    Surface: Graphics.TBitmap;
    function GetEffectImage(const I: Integer): Graphics.TBitmap;
  public
    Image: array [TEffectEnum] of Graphics.TBitmap;
    constructor Create;
    destructor Destroy; override;
    procedure Render;
  end;

implementation

uses
  Windows,
  Trollhunter.Error,
  Trollhunter.Graph,
  Trollhunter.TempSys,
  Trollhunter.PC,
  Trollhunter.Game,
  Trollhunter.Creatures,
  Trollhunter.Utils;

{ TEffects }

constructor TEffects.Create;
var
  I: TEffectEnum;
  Tileset: Graphics.TBitmap;
begin
  Surface := Graphics.TBitmap.Create;
  Tileset := Graphics.TBitmap.Create;
  Tileset.Handle := Windows.LoadBitmap(hInstance, 'EFFECTS');
  for I := Low(TEffectEnum) to High(TEffectEnum) do
  begin
    Image[I] := Graphics.TBitmap.Create;
    Graph.BitmapFromTileset(Image[I], Tileset, Ord(I));
    Image[I].Transparent := False;
  end;
  Tileset.Free;
end;

destructor TEffects.Destroy;
var
  I: TEffectEnum;
begin
  for I := Low(TEffectEnum) to High(TEffectEnum) do
    Image[I].Free;
  Surface.Free;
  inherited;
end;

function TEffects.GetEffectImage(const I: Integer): Graphics.TBitmap;
var
  S: string;
  N: TEffectEnum;
begin
  S := Creatures.PC.TempSys.VarName(I);
  for N := Low(TEffectEnum) to High(TEffectEnum) do
    if (S = EffectStr[N]) then
    begin
      Result := Image[N];
      Break;
    end;
  ScaleBMP(Result, TileSize, TileSize);
end;

procedure TEffects.Render;
var
  I, L: Integer;
begin
  if Creatures.PC.TempSys.Count > 0 then
  begin
    Surface.Width := Creatures.PC.TempSys.Count * TileSize;
    Surface.Height := TileSize;
    for I := 0 to Creatures.PC.TempSys.Count - 1 do
      Surface.Canvas.Draw(I * TileSize, 0, GetEffectImage(I));
    L := ((Graph.Width - Graph.PW) div 2) - (Surface.Width div 2);
    Graph.Surface.Canvas.Draw(L, Graph.Surface.Canvas.Font.Size + 8, Surface);
  end;
end;

end.
