unit uGame;

interface

uses uEntity, uMap, uLanguage;

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

const
  clBlack = $00000000;
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

  clDarkestRed = $FF330000;

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
  clLife: Cardinal = $FF990000;
  clMana: Cardinal = $FF000099;

var
  clAlarm: string = 'Lightest Green';

type
  TDifficulty = (dfEasy, dfNormal, dfHard, dfHell);
  TSpawn = class(TEntity);

type
  TAPOptionEnum = (apCoin, apFood, apRune, apGem, apBook, apPotion, apScroll,
    apKey, apPlant);

type
  TGame = class(TObject)
  private
    FDifficulty: TDifficulty;
    FTimer: Byte;
    FWon: Boolean;
    FMode: Boolean;
    FWizard: Boolean;
    FCanClose: Boolean;
    FShowMap: Boolean;
    FMkLang: Boolean;
    FLCorpses: Boolean;
    FScreenshot: string;
    FSpawn: TSpawn;
    FPortal: TSpawn;
    FPortalMap: TMapEnum;
    FPortalTile: TTileEnum;
    FShowEffects: Boolean;
    FAPOption: array [TAPOptionEnum] of Boolean;
    FLanguage: TLanguage;
  public
    constructor Create;
    destructor Destroy; override;
    property Difficulty: TDifficulty read FDifficulty write FDifficulty;
    property Timer: Byte read FTimer write FTimer;
    property Won: Boolean read FWon write FWon;
    property IsMode: Boolean read FMode write FMode;
    property Wizard: Boolean read FWizard write FWizard;
    property MkLang: Boolean read FMkLang write FMkLang;
    property CanClose: Boolean read FCanClose write FCanClose;
    property ShowMap: Boolean read FShowMap write FShowMap;
    property LCorpses: Boolean read FLCorpses write FLCorpses;
    property Screenshot: string read FScreenshot write FScreenshot;
    property Spawn: TSpawn read FSpawn write FSpawn;
    property Portal: TSpawn read FPortal write FPortal;
    function GetPath(SubDir: string = ''): string;
    function GetStrDifficulty: string;
    function GetVersion: string;
    property PortalMap: TMapEnum read FPortalMap write FPortalMap;
    property PortalTile: TTileEnum read FPortalTile write FPortalTile;
    property ShowEffects: Boolean read FShowEffects write FShowEffects;
    function GetTitle: string;
    procedure LoadConfig;
    procedure Start;
    procedure Log(S: string);
    function GetCursor: string;
    function IfThen(AValue: Boolean; const ATrue: string;
      const AFalse: string): string;
    function GetOption(I: TAPOptionEnum): Boolean;
    procedure ChOption(I: TAPOptionEnum);
    property Language: TLanguage read FLanguage;
  end;

var
  Game: TGame;

implementation

uses SysUtils, uPlayer, uMsgLog, uScenes,
  BearLibTerminal, uItem, uMob, uTerminal, uShop, uSpellbook;

{ TGame }

constructor TGame.Create;
var
  I: Byte;
  J: TAPOptionEnum;
begin
  Randomize;
  Timer := 0;
  Won := False;
  IsMode := False;
  Wizard := False;
  MkLang := False;
  for J := Low(TAPOptionEnum) to High(TAPOptionEnum) do
    FAPOption[J] := True;
  CanClose := False;
  ShowEffects := False;
  ShowMap := True;
  LCorpses := True;
  Difficulty := dfNormal;
  FSpawn := TSpawn.Create;
  FPortal := TSpawn.Create;
  PortalMap := deDarkWood;
  PortalTile := teStoneFloor;
  FLanguage := TLanguage.Create;
  for I := 1 to ParamCount do
  begin
    if (LowerCase(ParamStr(I)) = '-w') then
      Wizard := True;
    if (LowerCase(ParamStr(I)) = '-l') then
      MkLang := True;
  end;
end;

destructor TGame.Destroy;
begin
  if MkLang then
    Language.SaveLanguage();
  FreeAndNil(FLanguage);
  FreeAndNil(FPortal);
  FreeAndNil(FSpawn);
  inherited;
end;

function TGame.IfThen(AValue: Boolean; const ATrue: string;
  const AFalse: string): string;
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

function TGame.GetOption(I: TAPOptionEnum): Boolean;
begin
  Result := FAPOption[I]
end;

function TGame.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

function TGame.GetStrDifficulty: string;
begin
  case Difficulty of
    dfEasy:
      Result := _('Easy');
    dfNormal:
      Result := _('Normal');
    dfHard:
      Result := _('Hard');
  else
    Result := Terminal.Colorize(_('Hell'), 'Red');
  end;

end;

function TGame.GetVersion: string;
begin
  Result := '0.12.1';
end;

procedure TGame.Log(S: string);
begin
  terminal_set('Log: ' + S);
end;

procedure TGame.LoadConfig;
begin
  // Localization
  Language.UseLanguage(terminal_get('ini.localization.language'));
  // Load colors
  clDefault := Terminal.GetColorFromIni('Default', 'Yellow');
  clBackground := Terminal.GetColorFromIni('Background', 'Black');
  clCorpse := Terminal.GetColorFromIni('Corpse', 'Gray');
  clLook := Terminal.GetColorFromIni('Look', 'Yellow');
  clBkMob := Terminal.GetColorFromIni('BkMob', 'Darkest Red');
  clPlayer := Terminal.GetColorFromIni('Player', 'Yellow');
  clBkPlayer := Terminal.GetColorFromIni('BkPlayer', 'Darkest Green');
  clAlarm := Terminal.GetColorFromIni('Alarm');
  clLife := Terminal.GetColorFromIni('LifeBar', 'Life');
  clMana := Terminal.GetColorFromIni('ManaBar', 'Mana');
end;

procedure TGame.ChOption(I: TAPOptionEnum);
begin
  FAPOption[I] := not FAPOption[I];
end;

procedure TGame.Start;
begin
  Player.Clear;
  //
  Player.Skills.Start;
  Player.Start;
  Player.Calc;
  Player.Fill;
  //
  Spellbook.Start;
  Shops.New;
  // Intro
  MsgLog.Clear;
  MsgLog.Add(Terminal.Colorize(Format('%s %s %s', [_('Welcome to Elvion!'),
    _('You need to find and kill The King Troll!'), _('Press ? for help.')]),
    clAlarm));
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
