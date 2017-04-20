unit uGame;

interface

type
  TGame = class(TObject)
  private
    FWon: Boolean;
    FMode: Boolean;
    FWizard: Boolean;
    FCanClose: Boolean;
    FScreenshot: string;
  public
    constructor Create;
    destructor Destroy; override;
    property Won: Boolean read FWon write FWon;
    property IsMode: Boolean read FMode write FMode;
    property Wizard: Boolean read FWizard write FWizard;
    property CanClose: Boolean read FCanClose write FCanClose;
    property Screenshot: string read FScreenshot write FScreenshot;
    procedure LoadConfig;
    procedure Start;
    procedure Log(S: string);
  end;

var
  Game: TGame;

implementation

uses SysUtils, Math, Dialogs, uCommon, uPlayer, uMsgLog, uScenes, gnugettext,
  BearLibTerminal, uItem, uMob;

{ TGame }

constructor TGame.Create;
var
  I: Byte;
begin
  Randomize;
  Won := False;
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

procedure TGame.Log(S: string);
begin
  terminal_set('Log: ' + S);
end;

procedure TGame.LoadConfig;
begin
  // Localization
  UseLanguage(terminal_get('ini.localization.locale'));
  // Load colors
  clDefault := color_from_name(GetColorFromIni('Default'));
  clBackground := color_from_name(GetColorFromIni('Background'));
  clCorpse := color_from_name(GetColorFromIni('Corpse'));
  clLook := color_from_name(GetColorFromIni('Look'));
  clBkMob := color_from_name(GetColorFromIni('BkMob'));
  clPlayer := color_from_name(GetColorFromIni('Player'));
  clBkPlayer := color_from_name(GetColorFromIni('BkPlayer'));
  clAlarm := GetColorFromIni('Alarm');
  // Save to log
  Game.Log(Format('Items: count=%d', [Length(ItemBase)]));
  Game.Log(Format('Mobs: count=%d', [Length(MobBase)]));
end;

procedure TGame.Start;
begin
  IsMode := True;
  Player.SkillSet;
  Player.StarterSet;
  // Intro
  MsgLog.Clear;
  MsgLog.Add(Format(FC, [clAlarm, Format('%s %s %s', [
    _('Welcome to Elvion!'),
    _('You need to find and kill The King Troll!'),
    _('Press ? for help.')])]));
  //
  Scenes.SetScene(scGame);
end;

initialization  
  Game := TGame.Create; 

finalization
  FreeAndNil(Game);

end.
