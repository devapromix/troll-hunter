unit Trollhunter.Scene.BaseMenu;

interface

uses
  Classes,
  Trollhunter.Scene;

type
  TSceneBaseMenu = class(TScene)
  private
    FID: Word;
  public
    procedure Render(); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    constructor Create(TitleLangID: Word);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Trollhunter.Lang,
  Trollhunter.Graph,
  Trollhunter.Scenes,
  Trollhunter.Scene.Menu;

{ TSceneBaseMenu }

constructor TSceneBaseMenu.Create(TitleLangID: Word);
begin
  FID := Word(TitleLangID);
end;

destructor TSceneBaseMenu.Destroy;
begin

  inherited;
end;

procedure TSceneBaseMenu.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if (Key = 27) or (Key = 123) then
    Scenes.Scene := SceneMenu;
end;

procedure TSceneBaseMenu.KeyPress(var Key: Char);
begin
  inherited;

end;

procedure TSceneBaseMenu.Render;
begin
  inherited;
  Graph.Clear(0);
  Graph.Text.TitleOut(GetLang(FID));
  Graph.Text.BarOut('esc', GetLang(29), True);
end;

end.
