unit Trollhunter.Scenes;

interface

uses
  Classes,
  Trollhunter.Scene;

type
  TScenes = class(TScene)
  private
    FScene: TScene;
    procedure SetScene(const Value: TScene);
  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    property Scene: TScene read FScene write SetScene default nil;
    procedure Clear;
  end;

var
  Scenes: TScenes;

implementation

uses
  Trollhunter.Screenshot;

{ TScenes }

procedure TScenes.Clear;
begin
  Scene := nil;
end;

procedure TScenes.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    118: // F7
      begin
        TakeScreenShot;
        Exit;
      end;
  end;
  if (Scene <> nil) then
    Scene.KeyDown(Key, Shift);
end;

procedure TScenes.KeyPress(var Key: Char);
begin
  if (Scene <> nil) then
    Scene.KeyPress(Key);
end;

procedure TScenes.Render;
begin
  if (Scene <> nil) then
    Scene.Render;
end;

procedure TScenes.SetScene(const Value: TScene);
begin
  FScene := Value;
  Render;
end;

initialization

Scenes := TScenes.Create;

finalization

Scenes.Free;

end.
