unit Trollhunter.Scene.Help;

interface

uses
  Classes,
  Graphics,
  Trollhunter.Scene,
  Trollhunter.Scenes,
  Trollhunter.Scene.BaseGame;

type
  TSceneHelp = class(TSceneBaseGame)
  private
    X, Y: Integer;
    procedure Add(K: string; L: Word); overload;
  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneHelp: TSceneHelp;

implementation

uses
  SysUtils,
  Trollhunter.Graph,
  Trollhunter.Error,
  Trollhunter.Lang,
  Trollhunter.Color;

{ TSceneHelp }

procedure TSceneHelp.Add(K: string; L: Word);
var
  W: Word;
begin
  with Graph.Surface.Canvas do
  begin
    W := Graph.Surface.Width div 3;
    Font.Style := [fsBold];
    Font.Color := cLtBrown;
    TextOut((Graph.CharWidth * 1) + (W * X), Y * Graph.CharHeight,
      '[' + K + ']');
    Font.Style := [];
    Font.Color := cBgColor;
    TextOut((Graph.CharWidth * 6) + (W * X), Y * Graph.CharHeight, GetLang(L));
    Inc(X);
    if (X > 2) then
    begin
      X := 0;
      Inc(Y);
    end;
  end;
end;

constructor TSceneHelp.Create;
begin
  inherited Create(125);
end;

destructor TSceneHelp.Destroy;
begin

  inherited;
end;

procedure TSceneHelp.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

procedure TSceneHelp.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneHelp.Render;
begin
  inherited;
  X := 0;
  Y := 2;
  try
    Add('I', 25);
    Add('C', 8);
    Add('G', 21);
    Add('U', 98);
    Add('L', 121);
    Add('S', 122);
    Add('W', 123);
    Add('R', 124);
    Add('F1', 125);
    Add('D', 209);
    Add('F7', 126);
    Graph.Render;
  except
    on E: Exception do
      Error.Add('SceneHelp.Render', E.Message);
  end;

end;

initialization

SceneHelp := TSceneHelp.Create;

finalization

SceneHelp.Free;

end.
