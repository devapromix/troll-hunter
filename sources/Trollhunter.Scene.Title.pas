unit Trollhunter.Scene.Title;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneTitle = class(TScene)
  private
    FCur: UInt;
    FCount: UInt;
    procedure Load;
  public
    constructor Create;
    procedure Render; override;
    procedure Update(var Key: UInt); override;
    procedure RenderHeroes();
  end;

implementation

{ TSceneTitle }

uses
  Math,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.Game,
  Trollhunter.UI,
  Trollhunter.UI.Logo,
  Trollhunter.Helpers,
  Trollhunter.Language,
  Trollhunter.Scene.Load;

constructor TSceneTitle.Create;
begin
  FCur := 0;
  FCount := 0;
end;

procedure TSceneTitle.Load;
begin
  (Scenes.GetScene(scLoad) as TSceneLoad).IsLoad := True;
  Scenes.SetScene(scLoad);
  Terminal.Refresh;
  Terminal_Delay(1000);
  Scenes.SetScene(scTitle); // עטלקאסמגמ
end;

procedure TSceneTitle.Render;
begin
  Logo.Render(True);
  Terminal.Print(Screen.Width - ((Screen.Width div 2) - (Logo.Width div 2) + 2),
    14, Format('by Apromix v.%s', [Game.GetVersion]), TK_ALIGN_RIGHT);
  RenderHeroes;
  if Mode.Wizard then
  begin
    Self.AddKey('Space', _('Create a new hero'));
    Self.AddKey('Z', Terminal.Colorize(_('Turn Wizard Mode Off'), 'Red'), True);
  end
  else
    Self.AddKey('Space', _('Create a new hero'), True);
end;

procedure TSceneTitle.RenderHeroes;
const
  L = 12;
  T = 15;
var
  I: UInt;
begin
  FCount := 5;
  if (FCount = 0) then
    Exit;

  Terminal.ForegroundColor(clWhite);
  Terminal.Print(L + 4, T, _('Which hero shall you play?'));

  for I := 0 to FCount - 1 do
  begin
    Terminal.Print(L, T + I + 2, UI.MenuItem(Chr(I + 65), '==========',
      FCur = I));
  end;
end;

procedure TSceneTitle.Update(var Key: UInt);
begin
  case Key of
    TK_UP, TK_KP_8:
      begin
        if (FCount = 0) then
          Exit;
        if FCur > 0 then
          FCur := Pred(FCur);
      end;
    TK_DOWN, TK_KP_2:
      begin
        if (FCount = 0) then
          Exit;
        if FCur < FCount - 1 then
          FCur := Succ(FCur);
      end;
    TK_ESCAPE:
      Game.CanClose := True;
    TK_A .. TK_J:
      begin
        if (Key - (TK_A) > FCount - 1) then
          Exit;
        FCur := Key - (TK_A);
        Load;
      end;
    TK_SPACE:
      Scenes.SetScene(scDifficulty);
    TK_ENTER, TK_KP_ENTER:
      Load;
    TK_Z:
      Mode.Wizard := False;
  end;
end;

end.
