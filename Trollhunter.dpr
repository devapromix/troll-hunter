program Trollhunter;

uses
  SysUtils,
  BearLibTerminal in 'BearLibTerminal.pas',
  uTerminal in 'uTerminal.pas',
  uCommon in 'uCommon.pas',
  uScenes in 'uScenes.pas',
  uPlayer in 'uPlayer.pas',
  uMap in 'uMap.pas';

var
  Key: Word = 0;
  IsRender: Boolean = True;

begin
  repeat
    if IsRender then Scenes.Render;
    Key := 0;
    if terminal_has_input() then
    begin
      Key := terminal_read();
      Scenes.Update(Key);
      IsRender := True;
      Continue;
    end;
    if IsRender then Terminal.Refresh;
    terminal_delay(10);
    IsRender := False;
  until (Key = TK_CLOSE);
end.

