unit Trollhunter.Scene.Final;

interface

uses
  Trollhunter.Types,
  Trollhunter.Scenes;

type
  TSceneFinal = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

{ TSceneFinal }

uses
  SysUtils,
  BearLibTerminal,
  Trollhunter.Terminal,
  Trollhunter.UI,
  Trollhunter.UI.Log,
  Trollhunter.UI.Logo,
  Trollhunter.Language,
  Trollhunter.Player,
  Trollhunter.Game;

procedure TSceneFinal.Render;
begin
  inherited Render;
  case Scenes.FinalEnum of
    feWon:
      begin
        Logo.Render(False);
        Terminal.Print(CX, CY + 1, UpperCase(_('Congratulations!!!')), TK_ALIGN_CENTER);
        Terminal.Print(CX, CY + 3, Format(_('You have won. Press %s'), [UI.KeyToStr('ENTER')]), TK_ALIGN_CENTER);
      end;
    feQuit:
      begin
        Logo.Render(False);
        Terminal.Print(CX, CY + 3, Format(_('Do you wish to quit? %s/%s'), [UI.KeyToStr('Y'), UI.KeyToStr('N')]), TK_ALIGN_CENTER);
      end;
    feDefeat:
      begin
        Logo.Render(False);
        Terminal.Print(CX, CY + 1, UpperCase(_('Game over!!!')), TK_ALIGN_CENTER);
        if (Player.Killer = '') then
          Terminal.Print(CX, CY + 3, Format(_('You dead. Press %s'), [UI.KeyToStr('ENTER')]), TK_ALIGN_CENTER)
        else
          Terminal.Print(CX, CY + 3, Format(_('You were slain by %s. Press %s'), [Terminal.Colorize(Player.Killer, clAlarm), UI.KeyToStr('ENTER')]),
            TK_ALIGN_CENTER);
        if Mode.Wizard then
        begin
          Terminal.Print(CX, CY + 5, Terminal.Colorize(_('Wizard Mode'), 'Red') + ' ' + Format(_('Press %s to continue...'), [UI.KeyToStr('SPACE')]),
            TK_ALIGN_CENTER);
        end;
      end;
  end;
end;

procedure TSceneFinal.Update(var Key: UInt);
begin
  case Scenes.FinalEnum of
    feWon:
      case Key of
        TK_ENTER, TK_KP_ENTER:
          begin
            Player.SaveCharacterDump(_('Won the game'));
            Game.CanClose := True;
          end;
      end;
    feQuit:
      case Key of
        TK_Y:
          begin
            Player.SaveCharacterDump(_('Quit the game'));
            Game.Save;
            Game.CanClose := True;
          end;
        TK_ESCAPE, TK_N:
          Scenes.GoBack;
      end;
    feDefeat:
      case Key of
        TK_ENTER, TK_KP_ENTER:
          begin
            Game.DeleteCurrentSaveFile;
            Player.SaveCharacterDump(Format(_('Killed by %s'), [Player.Killer]));
            Game.CanClose := True;
          end;
        TK_SPACE:
          if Mode.Wizard then
          begin
            Player.Fill;
            Scenes.SetScene(scGame);
          end;
      end;
  end;
end;

end.
