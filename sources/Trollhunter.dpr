program Trollhunter;

uses
  Windows,
  Forms,
  Trollhunter.MainForm in 'Forms\Trollhunter.MainForm.pas' {MainForm} ,
  Trollhunter.Scenes in 'Scenes\Trollhunter.Scenes.pas',
  Trollhunter.Scene in 'Scenes\Trollhunter.Scene.pas',
  uSceneGame in 'uSceneGame.pas',
  Trollhunter.Creatures in 'Trollhunter.Creatures.pas',
  Trollhunter.Graph in 'Trollhunter.Graph.pas',
  Trollhunter.Craft in 'Trollhunter.Craft.pas',
  Trollhunter.Map in 'Trollhunter.Map.pas',
  uSceneMenu in 'uSceneMenu.pas',
  Trollhunter.Utils in 'Trollhunter.Utils.pas',
  Trollhunter.Item in 'Trollhunter.Item.pas',
  Trollhunter.Trap in 'Trollhunter.Trap.pas',
  Trollhunter.Script in 'Trollhunter.Script.pas',
  Trollhunter.Tile in 'Trollhunter.Tile.pas',
  Trollhunter.Screenshot in 'Trollhunter.Screenshot.pas',
  Trollhunter.Log in 'Trollhunter.Log.pas',
  uSceneAbout in 'uSceneAbout.pas',
  Trollhunter.Game in 'Trollhunter.Game.pas',
  uSceneName in 'uSceneName.pas',
  uSceneRecords in 'uSceneRecords.pas',
  uSceneLoad in 'uSceneLoad.pas',
  Trollhunter.Scores in 'Trollhunter.Scores.pas',
  Trollhunter.Color in 'Trollhunter.Color.pas',
  Trollhunter.Name in 'Trollhunter.Name.pas',
  Trollhunter.Map.Generator in 'Trollhunter.Map.Generator.pas',
  Trollhunter.AStar in 'Trollhunter.AStar.pas',
  Trollhunter.Error in 'Trollhunter.Error.pas',
  uSceneLevelUp in 'uSceneLevelUp.pas',
  Trollhunter.Inv in 'Trollhunter.Inv.pas',
  uSceneInv in 'uSceneInv.pas',
  uSceneItem in 'uSceneItem.pas',
  Trollhunter.Projectiles in 'Trollhunter.Projectiles.pas',
  Trollhunter.TempSys in 'Trollhunter.TempSys.pas',
  uSceneItems in 'uSceneItems.pas',
  Trollhunter.Decorator in 'Trollhunter.Decorator.pas',
  uSceneChar in 'uSceneChar.pas',
  uSceneSettings in 'uSceneSettings.pas',
  Trollhunter.Settings in 'Trollhunter.Settings.pas',
  Trollhunter.Lang in 'Trollhunter.Lang.pas',
  Trollhunter.Light in 'Trollhunter.Light.pas',
  Trollhunter.Effect in 'Trollhunter.Effect.pas',
  Trollhunter.Look in 'Trollhunter.Look.pas',
  Trollhunter.Resources in 'Trollhunter.Resources.pas',
  uSceneBaseMenu in 'uSceneBaseMenu.pas',
  uSceneBaseGame in 'uSceneBaseGame.pas',
  Trollhunter.Time in 'Trollhunter.Time.pas',
  uSceneHelp in 'uSceneHelp.pas',
  uSceneRace in 'uSceneRace.pas',
  Trollhunter.Race in 'Trollhunter.Race.pas',
  Trollhunter.Skill in 'Trollhunter.Skill.pas',
  Trollhunter.Entity in 'Trollhunter.Entity.pas',
  Trollhunter.Bar in 'Trollhunter.Bar.pas',
  Trollhunter.MiniMap in 'Trollhunter.MiniMap.pas',
  Trollhunter.CustomMap in 'Trollhunter.CustomMap.pas',
  Trollhunter.RandItems in 'Trollhunter.RandItems.pas',
  uSceneIntro in 'uSceneIntro.pas',
  Trollhunter.PC in 'Trollhunter.PC.pas',
  Trollhunter.BaseCreature in 'Trollhunter.BaseCreature.pas',
  Trollhunter.Creature in 'Trollhunter.Creature.pas',
  Trollhunter.Enemy in 'Trollhunter.Enemy.pas',
  Trollhunter.Formulas in 'Trollhunter.Formulas.pas',
  Trollhunter.GlobalMap in 'Trollhunter.GlobalMap.pas',
  Trollhunter.Statistics in 'Trollhunter.Statistics.pas',
  uSceneStatistics in 'uSceneStatistics.pas',
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
