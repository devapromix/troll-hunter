program Trollhunter;

uses
  SysUtils,
  Dialogs,
  gnugettext in 'gnugettext.pas',
  BearLibTerminal in 'BearLibTerminal.pas',
  BeaRLibItems in 'BeaRLibItems.pas',
  uTerminal in 'uTerminal.pas',
  uCommon in 'uCommon.pas',
  uScenes in 'uScenes.pas',
  uPlayer in 'uPlayer.pas',
  uMap in 'uMap.pas',
  uVillage in 'uVillage.pas',
  uItem in 'uItem.pas',
  uMob in 'uMob.pas',
  uGame in 'uGame.pas',
  uMsgLog in 'uMsgLog.pas',
  uCorpse in 'uCorpse.pas';

var
  Key: Word = 0;
  IsRender: Boolean = True;

begin
  Game.LoadConfig;
  repeat
    if IsRender then                
    begin
      Scenes.Render;
      Terminal.Refresh;
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
