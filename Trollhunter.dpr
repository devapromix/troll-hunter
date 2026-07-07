{$IFDEF FPC}
{$IFDEF Windows}
{$APPTYPE GUI}
{$ENDIF}
{$ENDIF}

program Trollhunter;

uses
  Dialogs,
  SysUtils,
  BearLibTerminal,
  Trollhunter.Types,
  Trollhunter.Game,
  Trollhunter.Terminal,
  Trollhunter.Scenes,
  Trollhunter.Helpers;

var
  Key: UInt = 0;
  IsRender: boolean = True;

begin
  Randomize();
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
