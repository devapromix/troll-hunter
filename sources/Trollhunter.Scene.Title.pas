unit Trollhunter.Scene.Title;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneTitle = class(TScene)
  private const
    MaxRows = 10;
  private
    FCur: UInt;
    FTop: UInt;
    FCount: UInt;
    procedure Load(FileName: string);
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
  Trollhunter.Scene.Load,
  Trollhunter.Mods;

constructor TSceneTitle.Create;
begin
  FCur := 0;
  FTop := 0;
  FCount := 0;
end;

procedure TSceneTitle.Load(FileName: string);
begin
  (Scenes.GetScene(scLoad) as TSceneLoad).IsLoad := True;
  Scenes.SetScene(scLoad);
  Terminal.Refresh;
  Terminal_Delay(1000);
  Game.Load(FileName);
end;

procedure TSceneTitle.Render;
begin
  Logo.Render(True);
  Terminal.Print(Terminal.Screen.Width - ((Terminal.Screen.Width div 2) -
    (Logo.Width div 2) + 2), 14, Format('v.%s', [Game.GetVersion]),
    TK_ALIGN_RIGHT);
  RenderHeroes;
  if Mode.Wizard then
  begin
    Self.AddKey('Space', _('Create a new hero'));
    Self.AddKey('Tab', Terminal.Colorize(_('Turn Wizard Mode Off'), 'Red'));
  end
  else
    Self.AddKey('Space', _('Create a new hero'));
  Self.AddKey('1', _('Credits'), True);
end;

procedure TSceneTitle.RenderHeroes;
const
  L = 12;
  T = 15;
var
  I: UInt;
begin
  FCount := EnsureRange(Game.SaveFL.Count, 0, 26);
  if (FCount = 0) then
    Exit;

  Terminal.ForegroundColor(clWhite);
  Terminal.Print(L, T, _('Which hero shall you play?'));

  for I := FTop to Min(FCount, MaxRows) + FTop - 1 do
  begin
    Terminal.Print(L, T + I + 2 - FTop, UI.MenuItem(Chr(I + 65), Game.SaveTL[I],
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
        if (FCur > 0) then
          FCur := Pred(FCur);
        if (FTop > 0) and (FCur = FTop - 1) then
          FTop := Pred(FTop);
      end;
    TK_DOWN, TK_KP_2:
      begin
        if (FCount = 0) then
          Exit;
        if (FCur < FCount - 1) then
          FCur := Succ(FCur);
        if (FTop < FCount - 1) and (FCur = FTop + MaxRows) then
          FTop := Succ(FTop);
      end;
    TK_ESCAPE:
      Game.CanClose := True;
    TK_A .. TK_Z:
      begin
        if (Key - (TK_A) > FCount - 1) then
          Exit;
        FCur := Key - (TK_A);
        Load(Game.SaveFL[FCur]);
      end;
    TK_SPACE:
      begin
        GMods.LoadMods;
        Game.GenSaveFileName;
        Sleep(100);
        Scenes.SetScene(scMods);
      end;
    TK_ENTER, TK_KP_ENTER:
      if Game.SaveFL.Count > 0 then
        Load(Game.SaveFL[FCur]);
    TK_TAB:
      begin
        Mode.Wizard := False;
        Game.SetWindowTitle;
      end;
    TK_1:
      Scenes.SetScene(scCredits);
  end;
end;

end.
