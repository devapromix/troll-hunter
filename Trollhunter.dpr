{$IFDEF FPC}
{$IFDEF Windows}
{$APPTYPE GUI}
{$ENDIF}
{$ENDIF}

program Trollhunter;

uses
  Dialogs, SysUtils, BearLibTerminal, Trollhunter.Types, Trollhunter.Game,
  Trollhunter.Terminal, Trollhunter.Scenes;

var
  Key: UInt = 0;
  IsRender: Boolean = True;
  CurrentTime: QWord;

begin
  Randomize();
  Game.LoadConfig();

  repeat
    CurrentTime := GetTickCount64();

    if (Game.RenderTimeout > 0) and
       (CurrentTime >= Game.RenderTimeout) then
    begin
      Game.RenderTimeout := 0;
      IsRender := True;
    end;

    if terminal_has_input() then
    begin
      Key := terminal_read();
      Scenes.Update(Key);
      IsRender := True;
    end;

    if IsRender then
    begin
      Scenes.Render();
      Terminal.Refresh();
      IsRender := False;
    end;

    terminal_delay(10);
  until Game.CanClose;

end.
