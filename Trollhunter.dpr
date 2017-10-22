{$IFDEF FPC}
{$IFDEF Windows}
{$APPTYPE GUI}
{$ENDIF}
{$ENDIF}
program Trollhunter;

uses
  SysUtils,
  Dialogs,
  BearLibTerminal in 'BearLibTerminal.pas',
  uGame in 'uGame.pas',
  uLanguage in 'uLanguage.pas',
  uTerminal in 'uTerminal.pas',
  uScenes in 'uScenes.pas',
  uPlayer in 'uPlayer.pas',
  uMap in 'uMap.pas',
  uItem in 'uItem.pas',
  uMob in 'uMob.pas',
  uMsgLog in 'uMsgLog.pas',
  uCorpse in 'uCorpse.pas',
  uEntity in 'uEntity.pas',
  uCalendar in 'uCalendar.pas',
  uShop in 'uShop.pas',
  uSpellbook in 'uSpellbook.pas',
  uTalent in 'uTalent.pas',
  uAbility in 'uAbility.pas',
  uSkill in 'uSkill.pas',
  uLogo in 'uLogo.pas',
  uStatistic in 'uStatistic.pas',
  uAffixes in 'uAffixes.pas',
  uCreature in 'uCreature.pas',
  uAttribute in 'uAttribute.pas',
  uUI in 'uUI.pas',
  uPathFind in 'uPathFind.pas',
  uBearLibItemsCommon in 'BearLibItems\uBearLibItemsCommon.pas',
  uBearLibItemsDungeon in 'BearLibItems\uBearLibItemsDungeon.pas',
  uBearLibItemsInventory in 'BearLibItems\uBearLibItemsInventory.pas';

var
  Key: Word = 0;
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
