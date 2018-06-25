{$IFDEF FPC}
{$IFDEF Windows}
{$APPTYPE GUI}
{$ENDIF}
{$ENDIF}
program Trollhunter;

uses
  SysUtils,
  Dialogs,
  Trollhunter.Types in 'Sources\Trollhunter.Types.pas',
  uGame in 'Sources\uGame.pas',
  uLanguage in 'Sources\uLanguage.pas',
  Trollhunter.Terminal in 'Sources\Trollhunter.Terminal.pas',
  uScenes in 'Sources\uScenes.pas',
  uPlayer in 'Sources\uPlayer.pas',
  uMap in 'Sources\uMap.pas',
  uItem in 'Sources\uItem.pas',
  uMob in 'Sources\uMob.pas',
  Trollhunter.UI.Log in 'Sources\Trollhunter.UI.Log.pas',
  uCorpse in 'Sources\uCorpse.pas',
  uEntity in 'Sources\uEntity.pas',
  uCalendar in 'Sources\uCalendar.pas',
  Trollhunter.Item.Shop in 'Sources\Trollhunter.Item.Shop.pas',
  uSpellbook in 'Sources\uSpellbook.pas',
  uTalent in 'Sources\uTalent.pas',
  uAbility in 'Sources\uAbility.pas',
  uSkill in 'Sources\uSkill.pas',
  Trollhunter.UI.Logo in 'Sources\Trollhunter.UI.Logo.pas',
  uStatistic in 'Sources\uStatistic.pas',
  Trollhunter.Item.Affixes in 'Sources\Trollhunter.Item.Affixes.pas',
  uCreature in 'Sources\uCreature.pas',
  uAttribute in 'Sources\uAttribute.pas',
  Trollhunter.UI in 'Sources\Trollhunter.UI.pas',
  BearLibTerminal in 'Sources\BearLibTerminal\BearLibTerminal.pas',
  uPathFind in 'Sources\BearLibPathFind\uPathFind.pas',
  uBearLibItemsCommon in 'Sources\BearLibItems\uBearLibItemsCommon.pas',
  uBearLibItemsDungeon in 'Sources\BearLibItems\uBearLibItemsDungeon.pas',
  uBearLibItemsInventory in 'Sources\BearLibItems\uBearLibItemsInventory.pas',
  uQuest in 'Sources\uQuest.pas',
  uHelpers in 'Sources\uHelpers.pas',
  uWeather in 'sources\uWeather.pas',
  uRace in 'sources\uRace.pas',
  uClass in 'sources\uClass.pas',
  Trollhunter.Scene.Enchant in 'sources\Trollhunter.Scene.Enchant.pas',
  Trollhunter.Scene.Name in 'sources\Trollhunter.Scene.Name.pas',
  Trollhunter.Scene.Rest in 'sources\Trollhunter.Scene.Rest.pas',
  Trollhunter.Scene.RacesAndClasses in 'sources\Trollhunter.Scene.RacesAndClasses.pas',
  Trollhunter.Scene.Quest in 'sources\Trollhunter.Scene.Quest.pas',
  Trollhunter.Utils in 'sources\Trollhunter.Utils.pas',
  Trollhunter.Scene.Background in 'sources\Trollhunter.Scene.Background.pas',
  Trollhunter.Item.Types in 'sources\Trollhunter.Item.Types.pas',
  Trollhunter.Player.Types in 'sources\Trollhunter.Player.Types.pas';

var
  Key: UInt = 0;
  IsRender: Boolean = True;

begin
  Randomize();
{$IFNDEF FPC}
{$IF COMPILERVERSION >= 18}
  ReportMemoryLeaksOnShutdown := True;
{$IFEND}
{$ENDIF}
  Game.LoadConfig();
  repeat
    if (Game.Timer > 0) then
    begin
      Game.Timer := Game.Timer - 1;
      if (Game.Timer = 0) then
        IsRender := True;
    end;
    if IsRender then
    begin
      Scenes.Render();
      Terminal.Refresh();
    end;
    Key := 0;
    if terminal_has_input() then
    begin
      Key := terminal_read();
      Scenes.Update(Key);
      IsRender := True;
      Continue;
    end;
    terminal_delay(10);
    IsRender := False;
  until Game.CanClose;

end.
