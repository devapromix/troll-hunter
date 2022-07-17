unit Trollhunter.Scene.About;

interface

uses
  Classes,
  Trollhunter.Scene;

type
  TSceneAbout = class(TScene)
  private

  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    constructor Create;
    destructor Destroy; override;
  end;

var
  SceneAbout: TSceneAbout;

implementation

{ TSceneAbout }

constructor TSceneAbout.Create;
begin

end;

destructor TSceneAbout.Destroy;
begin

  inherited;
end;

procedure TSceneAbout.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

end;

procedure TSceneAbout.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneAbout.Render;
begin
  inherited;

end;

initialization

SceneAbout := TSceneAbout.Create;

finalization

SceneAbout.Free;

end.
