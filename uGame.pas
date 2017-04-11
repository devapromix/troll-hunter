unit uGame;

interface

type
  TGame = class(TObject)
  private
    FMode: Boolean;
    FWizard: Boolean;
    FCanClose: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property IsMode: Boolean read FMode write FMode;
    property Wizard: Boolean read FWizard write FWizard;
    property CanClose: Boolean read FCanClose write FCanClose;
    procedure Start;
  end;

var
  Game: TGame;

implementation

uses SysUtils, Math, Dialogs, uCommon, uPlayer, uMsgLog, uScenes, gnugettext;

{ TGame }

constructor TGame.Create;
var
  I: Byte;
begin
  Randomize;
  IsMode := False;
  Wizard := False;
  CanClose := False;
  for I := 1 to ParamCount do
  begin
    if (LowerCase(ParamStr(I)) = '-w') then
      Wizard := True;
  end;
end;

destructor TGame.Destroy;
begin

  inherited;
end;

procedure TGame.Start;
begin
  IsMode := True;
  Player.SkillSet;
  Player.StarterSet;
  MsgLog.Clear;
  MsgLog.Add
    (_('Welcome to Elvion! You need to find and kill The King Troll! Press ? for help.'));
  Scenes.SetScene(scGame);
end;

initialization  
  Game := TGame.Create;

finalization
  FreeAndNil(Game);

end.
