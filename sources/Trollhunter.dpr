﻿program Trollhunter;

uses
  Windows,
  Forms,
  Trollhunter.MainForm in 'Forms\Trollhunter.MainForm.pas' {MainForm} ,
  Trollhunter.Scenes in 'Scenes\Trollhunter.Scenes.pas',
  Trollhunter.Scene in 'Scenes\Trollhunter.Scene.pas',
  uSceneGame in 'uSceneGame.pas',
  Trollhunter.Creatures in 'Trollhunter.Creatures.pas',
  Trollhunter.Graph in 'Trollhunter.Graph.pas',
  Trollhunter.Map in 'Trollhunter.Map.pas',
  uSceneMenu in 'uSceneMenu.pas',
  Trollhunter.Utils in 'Trollhunter.Utils.pas',
  Trollhunter.Item in 'Trollhunter.Item.pas',
  uTrap in 'uTrap.pas',
  uScript in 'uScript.pas',
  Trollhunter.Tile in 'Trollhunter.Tile.pas',
  uScreenshot in 'uScreenshot.pas',
  uLog in 'uLog.pas',
  uSceneAbout in 'uSceneAbout.pas',
  Trollhunter.Game in 'Trollhunter.Game.pas',
  uSceneName in 'uSceneName.pas',
  uSceneRecords in 'uSceneRecords.pas',
  uSceneLoad in 'uSceneLoad.pas',
  Trollhunter.Scores in 'Trollhunter.Scores.pas',
  Trollhunter.Color in 'Trollhunter.Color.pas',
  uName in 'uName.pas',
  uMapGenerator in 'uMapGenerator.pas',
  Trollhunter.AStar in 'Trollhunter.AStar.pas',
  Trollhunter.Error in 'Trollhunter.Error.pas',
  uSceneLevelUp in 'uSceneLevelUp.pas',
  uInv in 'uInv.pas',
  uSceneInv in 'uSceneInv.pas',
  uSceneItem in 'uSceneItem.pas',
  uProjectiles in 'uProjectiles.pas',
  uTempSys in 'uTempSys.pas',
  uSceneItems in 'uSceneItems.pas',
  uDecorator in 'uDecorator.pas',
  uSceneChar in 'uSceneChar.pas',
  uSceneSettings in 'uSceneSettings.pas',
  uSettings in 'uSettings.pas',
  uLang in 'uLang.pas',
  uLight in 'uLight.pas',
  uNews in 'uNews.pas',
  uEffect in 'uEffect.pas',
  uLook in 'uLook.pas',
  Trollhunter.Resources in 'Trollhunter.Resources.pas',
  uDocs in 'uDocs.pas',
  uSceneBaseMenu in 'uSceneBaseMenu.pas',
  uSceneBaseGame in 'uSceneBaseGame.pas',
  uTime in 'uTime.pas',
  uSceneHelp in 'uSceneHelp.pas',
  uSceneRace in 'uSceneRace.pas',
  uRace in 'uRace.pas',
  uSkill in 'uSkill.pas',
  Trollhunter.Entity in 'Trollhunter.Entity.pas',
  Trollhunter.Bar in 'Trollhunter.Bar.pas',
  Trollhunter.MiniMap in 'Trollhunter.MiniMap.pas',
  uCustomMap in 'uCustomMap.pas',
  uRandItems in 'uRandItems.pas',
  uSceneIntro in 'uSceneIntro.pas',
  uPC in 'uPC.pas',
  Trollhunter.BaseCreature in 'Trollhunter.BaseCreature.pas',
  Trollhunter.Creature in 'Trollhunter.Creature.pas',
  uEnemy in 'uEnemy.pas',
  uFormulas in 'uFormulas.pas',
  Trollhunter.GlobalMap in 'Trollhunter.GlobalMap.pas',
  uStatistics in 'uStatistics.pas',
  uSceneStatistics in 'uSceneStatistics.pas',
  uZip in 'uZip.pas',
  uTown in 'uTown.pas';

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
