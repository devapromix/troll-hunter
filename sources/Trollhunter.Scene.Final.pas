unit Trollhunter.Scene.Final;

interface

uses Trollhunter.Types,
  Trollhunter.Scenes;

type
  FinalEnum = (feQuit, feWin, feDefeat);

type
  TSceneFinal = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneFinal }

procedure TSceneFinal.Render;
begin
  inherited;

end;

procedure TSceneFinal.Update(var Key: UInt);
begin
  inherited;

end;

end.
