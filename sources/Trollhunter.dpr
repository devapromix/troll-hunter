program Trollhunter;

uses
  Windows,
  Forms,
  Trollhunter.MainForm in 'Forms\Trollhunter.MainForm.pas' {MainForm} ,
  Trollhunter.Scenes in 'Scenes\Trollhunter.Scenes.pas',
  Trollhunter.Scene in 'Scenes\Trollhunter.Scene.pas',
  Trollhunter.Scene.BaseMenu in 'Scenes\Trollhunter.Scene.BaseMenu.pas',
  Trollhunter.Scene.Menu in 'Scenes\Trollhunter.Scene.Menu.pas',
  Trollhunter.Scene.BaseGame in 'Scenes\Trollhunter.Scene.BaseGame.pas',
  Trollhunter.Scene.Game in 'Scenes\Trollhunter.Scene.Game.pas',
  Trollhunter.Scene.About in 'Scenes\Trollhunter.Scene.About.pas',
  Trollhunter.Scene.Inv in 'Scenes\Trollhunter.Scene.Inv.pas',
  Trollhunter.Scene.Name in 'Scenes\Trollhunter.Scene.Name.pas',
  Trollhunter.Scene.Records in 'Scenes\Trollhunter.Scene.Records.pas',
  Trollhunter.Scene.Load in 'Scenes\Trollhunter.Scene.Load.pas',
  Trollhunter.Scene.Items in 'Scenes\Trollhunter.Scene.Items.pas',
  Trollhunter.Scene.LevelUp in 'Scenes\Trollhunter.Scene.LevelUp.pas',
  Trollhunter.Scene.Item in 'Scenes\Trollhunter.Scene.Item.pas',
  Trollhunter.Scene.Char in 'Scenes\Trollhunter.Scene.Char.pas',
  Trollhunter.Scene.Settings in 'Scenes\Trollhunter.Scene.Settings.pas',
  Trollhunter.Scene.Intro in 'Scenes\Trollhunter.Scene.Intro.pas',
  Trollhunter.Scene.Statistics in 'Scenes\Trollhunter.Scene.Statistics.pas',
  Trollhunter.Scene.Help in 'Scenes\Trollhunter.Scene.Help.pas',
  Trollhunter.Scene.Race in 'Scenes\Trollhunter.Scene.Race.pas',
  Trollhunter.Creatures in 'Trollhunter.Creatures.pas',
  Trollhunter.Graph in 'Trollhunter.Graph.pas',
  Trollhunter.Craft in 'Trollhunter.Craft.pas',
  Trollhunter.Map in 'Trollhunter.Map.pas',
  Trollhunter.Utils in 'Trollhunter.Utils.pas',
  Trollhunter.Item in 'Trollhunter.Item.pas',
  Trollhunter.Trap in 'Trollhunter.Trap.pas',
  Trollhunter.Script in 'Trollhunter.Script.pas',
  Trollhunter.Map.Tiles in 'Trollhunter.Map.Tiles.pas',
  Trollhunter.Screenshot in 'Trollhunter.Screenshot.pas',
  Trollhunter.Log in 'Trollhunter.Log.pas',
  Trollhunter.Game in 'Trollhunter.Game.pas',
  Trollhunter.Scores in 'Trollhunter.Scores.pas',
  Trollhunter.Color in 'Trollhunter.Color.pas',
  Trollhunter.Name in 'Trollhunter.Name.pas',
  Trollhunter.Map.Generator in 'Trollhunter.Map.Generator.pas',
  Trollhunter.AStar in 'Trollhunter.AStar.pas',
  Trollhunter.Error in 'Trollhunter.Error.pas',
  Trollhunter.Inv in 'Trollhunter.Inv.pas',
  Trollhunter.Projectiles in 'Trollhunter.Projectiles.pas',
  Trollhunter.TempSys in 'Trollhunter.TempSys.pas',
  Trollhunter.Decorator in 'Trollhunter.Decorator.pas',
  Trollhunter.Settings in 'Trollhunter.Settings.pas',
  Trollhunter.Lang in 'Trollhunter.Lang.pas',
  Trollhunter.Light in 'Trollhunter.Light.pas',
  Trollhunter.Effect in 'Trollhunter.Effect.pas',
  Trollhunter.Look in 'Trollhunter.Look.pas',
  Trollhunter.Resources in 'Trollhunter.Resources.pas',
  Trollhunter.Time in 'Trollhunter.Time.pas',
  Trollhunter.Race in 'Trollhunter.Race.pas',
  Trollhunter.Skill in 'Trollhunter.Skill.pas',
  Trollhunter.Entity in 'Trollhunter.Entity.pas',
  Trollhunter.Bar in 'Trollhunter.Bar.pas',
  Trollhunter.MiniMap in 'Trollhunter.MiniMap.pas',
  Trollhunter.CustomMap in 'Trollhunter.CustomMap.pas',
  Trollhunter.RandItems in 'Trollhunter.RandItems.pas',
  Trollhunter.PC in 'Trollhunter.PC.pas',
  Trollhunter.BaseCreature in 'Trollhunter.BaseCreature.pas',
  Trollhunter.Creature in 'Trollhunter.Creature.pas',
  Trollhunter.Enemy in 'Trollhunter.Enemy.pas',
  Trollhunter.Formulas in 'Trollhunter.Formulas.pas',
  Trollhunter.GlobalMap in 'Trollhunter.GlobalMap.pas',
  Trollhunter.Statistics in 'Trollhunter.Statistics.pas',
  Trollhunter.Zip in 'Trollhunter.Zip.pas',
  Trollhunter.Town in 'Trollhunter.Town.pas';

{$R *.res}

var
  UniqueMapping: THandle;

begin
  UniqueMapping := CreateFileMapping($FFFFFFFF, nil, PAGE_READONLY, 0, 32,
    'm6gh7jq2lb6mbpfrwchmaltdr45');
  if UniqueMapping = 0 then
    Halt
  else if GetLastError = ERROR_ALREADY_EXISTS then
    Halt;
  Application.Initialize;
  Application.Title := 'Trollhunter';
  Application.CreateForm(TMainForm, MainForm);
  if ParamCraftDoc then
    Items.MakeCraftDoc;
  Application.Run;

end.
