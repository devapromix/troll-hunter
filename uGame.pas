unit uGame;

interface

uses uEntity;

{
var
  BattleJournals: array of string;

procedure BattleLog(s: string);
begin
  SetLength(BattleJournals, Length(BattleJournals) + 1);
  BattleJournals[Length(BattleJournals) - 1] := s;
end;

function BattleJournal: string;
var
  s: string;
  i: integer;
begin
  s := '';
  for i := max(0, Length(BattleJournals) - N_BTL_LOG_LINES)
    to Length(BattleJournals) - 1 do
    s := s + BattleJournals[i] + #10;
  SetLength(s, Length(s) - 1);
  Result := s;
end;
}

const
  FT = '== %s ==';
  FC = '[color=%s]%s[/color]';

const
  clWhite = $FFDDDDDD;

  clGray = $FF777777;
  clRed = $FFFF0000;
  clGreen = $FF00FF00;
  clBlue = $FF0000FF;
  clYellow = $FFFFFF00;

  clLightGray = $FF999999;
  clLightRed = $FFFF8888;
  clLightGreen = $FF88FF88;
  clLightBlue = $FF8888FF;
  clLightYellow = $FFFFFF88;

  clLightestGray = $FFBBBBBB;
  clLightestRed = $FFFFAAAA;
  clLightestGreen = $FFAAFFAA;
  clLightestBlue = $FFAAAAFF;
  clLightestYellow = $FFFFFFAA;

  clDarkGray = $FF222222;
  clDarkRed = $FF880000;
  clDarkGreen = $FF008800;
  clDarkBlue = $FF000088;
  clDarkYellow = $FF888800;

const
  clFog = $FF222222;

var
  clDefault: Cardinal = $FFFFFF00;
  clBackground: Cardinal = $00000000;
  clCorpse: Cardinal = $FF555555;
  clLook: Cardinal = $FFFFFF33;
  clBkMob: Cardinal = $FF330000;
  clPlayer: Cardinal = $FF009900;
  clBkPlayer: Cardinal = $FF003300;

var
  clAlarm: string = 'Lightest Green';

type
  TDifficulty = (dfEasy, dfNormal, dfHard, dfHell);
  TSpawn = class(TEntity);

type
  TGame = class(TObject)
  private
    FDifficulty: TDifficulty;
    FTimer: Byte;
    FWon: Boolean;
    FMode: Boolean;
    FWizard: Boolean;
    FAPCoin: Boolean;
    FAPFood: Boolean;
    FAPPotion: Boolean;
    FAPScroll: Boolean;
    FCanClose: Boolean;
    FShowMap: Boolean;
    FLCorpses: Boolean;
    FScreenshot: string;
    FSpawn: TSpawn;
  public
    constructor Create;
    destructor Destroy; override;
    property Difficulty: TDifficulty read FDifficulty write FDifficulty;
    property Timer: Byte read FTimer write FTimer;
    property Won: Boolean read FWon write FWon;
    property IsMode: Boolean read FMode write FMode;
    property Wizard: Boolean read FWizard write FWizard;
    property APCoin: Boolean read FAPCoin write FAPCoin;
    property APFood: Boolean read FAPFood write FAPFood;
    property APPotion: Boolean read FAPPotion write FAPPotion;
    property APScroll: Boolean read FAPScroll write FAPScroll;
    property CanClose: Boolean read FCanClose write FCanClose;
    property ShowMap: Boolean read FShowMap write FShowMap;
    property LCorpses: Boolean read FLCorpses write FLCorpses;
    property Screenshot: string read FScreenshot write FScreenshot;
    property Spawn: TSpawn read FSpawn write FSpawn;
    function GetPath(SubDir: string = ''): string;
    function GetVersion: string;
    function GetTitle: string;
    procedure LoadConfig;
    procedure Start;
    procedure Log(S: string);
    function GetCursor: string;
    function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string): string;
  end;


var
  Game: TGame;

implementation

uses SysUtils, Math, Dialogs, uPlayer, uMsgLog, uScenes, gnugettext,
  BearLibTerminal, uItem, uMob, uTerminal, uShop;

{ TGame }

constructor TGame.Create;
var
  I: Byte;
begin
  Randomize;
  Timer := 0;
  Won := False;
  IsMode := False;
  Wizard := False;
  APCoin := True;
  APFood := True;
  APPotion := True;
  APScroll := True;
  CanClose := False;
  ShowMap := True;
  LCorpses := True;
  Difficulty := dfNormal;
  Spawn := TSpawn.Create;
  for I := 1 to ParamCount do
  begin
    if (LowerCase(ParamStr(I)) = '-w') then
      Wizard := True;
  end;
end;

destructor TGame.Destroy;
begin
  Spawn.Free;
  inherited;
end;

function TGame.IfThen(AValue: Boolean; const ATrue: string; const AFalse: string): string;
begin
  if AValue then
    Result := ATrue
  else
    Result := AFalse;
end;

function TGame.GetCursor: string;
begin
  Result := '_';
end;

function TGame.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

function TGame.GetVersion: string;
begin
  Result := '1.0';
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
  clDefault := Terminal.GetColorFromIni('Default', 'Yellow');
  clBackground := Terminal.GetColorFromIni('Background', 'Black');
  clCorpse := Terminal.GetColorFromIni('Corpse', 'Gray');
  clLook := Terminal.GetColorFromIni('Look', 'Yellow');
  clBkMob := Terminal.GetColorFromIni('BkMob', 'Darkest Red');
  clPlayer := Terminal.GetColorFromIni('Player', 'Yellow');
  clBkPlayer := Terminal.GetColorFromIni('BkPlayer', 'Darkest Green');
  clAlarm := Terminal.GetColorFromIni('Alarm');
end;

procedure TGame.Start;
begin
  IsMode := True;
  Player.SkillSet;
  Player.StarterSet;
  Shops.New;
  // Intro
  MsgLog.Clear;
  MsgLog.Add(Format(FC, [clAlarm, Format('%s %s %s', [_('Welcome to Elvion!'),
    _('You need to find and kill The King Troll!'), _('Press ? for help.')])]));
  //
  Scenes.SetScene(scGame);
end;

function TGame.GetTitle: string;
begin
  Result := _('Trollhunter');
end;

initialization

Game := TGame.Create;

finalization

FreeAndNil(Game);

end.
