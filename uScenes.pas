unit uScenes;

interface

type
  TSceneEnum = (scGame);

type
  TScene = class(TObject)
    procedure Render; virtual; abstract;
    procedure Update(var Key: Word); virtual; abstract;
  end;

type
  TScenes = class(TScene)
  private
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render; override;
    procedure Update(var Key: Word); override;
  end;

var
  Scenes: TScenes = nil;

implementation

{ TScenes }

constructor TScenes.Create;
begin

end;

destructor TScenes.Destroy;
begin

  inherited;
end;

procedure TScenes.Render;
begin
  inherited;

end;

procedure TScenes.Update(var Key: Word);
begin
  inherited;

end;

initialization
  Scenes := TScenes.Create;

finalization
  Scenes.Free;
  Scenes := nil;

end.
