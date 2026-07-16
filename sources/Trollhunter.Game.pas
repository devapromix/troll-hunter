unit Trollhunter.Game;

interface

uses
  Trollhunter.Types,
  Trollhunter.Entity,
  Trollhunter.Map;

{
  "Berserk" : "While berserk, combatant will get an extra attack (or spell cast) each turn."
  "Blessed" : "A blessed combatant suffers less damage from physical and magical attacks."
  "Blinded" : "When blinded, combatant's chance to hit when attack will be reduced considerably."
  "Eagle-eyed" : "An eagle-eyed combatant is more likely to hit with physical attacks."
  "Paralyzed" : "When paralyzed, a combatant cannot do anything and its turn is skipped."
  "Poisoned" : "A poisoned combatant suffers poison damage each turn, although it will not die from it."
  "Protected" : "A protected combatant is harder to hit with physical attacks."
  "Quick" : "A quick combatant can take additional steps when moving each turn."
  "Resistant" : "A resistant combatant is less affected by magical attacks."
  "Rooted" : "A rooted combatant cannot move, although it may still attack and cast spells."
  "Sluggish" : "A sluggish combatant is easier to hit with phsyical attacks."
  "Strong" : "A strong combatant inflicts more damage with physical attacks."
  "Weakened" : "A weakened combatant inflicts less damage with physical attacks."
}

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

  clLighterGray = $FFAAAAAA;
  clLighterRed = $FFFFAAAA;
  clLighterGreen = $FFAAFFAA;
  clLighterBlue = $FFAAAAFF;
  clLighterYellow = $FFFFFFAA;

  clLightestGray = $FFBBBBBB;
  clLightestRed = $FFFFCCCC;
  clLightestGreen = $FFCCFFCC;
  clLightestBlue = $FFCCCCFF;
  clLightestYellow = $FFFFFFCC;

  clDarkGray = $FF222222;
  clDarkRed = $FF880000;
  clDarkGreen = $FF008800;
  clDarkBlue = $FF000088;
  clDarkYellow = $FF888800;

  clDarkestRed = $FF330000;
  clDarkestGreen = $FF003300;

const
  clFog = $FF222222;

var
  clDefault: cardinal = $FFFFFF00;
  clBackground: cardinal = $00000000;
  clCorpse: cardinal = $FF555555;
  clLook: cardinal = $FFFFFF33;
  clBkMob: cardinal = $FF330000;
  clPlayer: cardinal = $FF009900;
  clBkPlayer: cardinal = $FF003300;
  clLife: cardinal = $FF990000;
  clMana: cardinal = $FF000099;

var
  clAlarm: string = 'Lightest Green';

type
  TMode = record
    Game: boolean;
    Wizard: boolean;
  end;

var
  Mode: TMode;

type
  TDifficulty = (dfEasy, dfNormal, dfHard, dfHell);

type
  TSpawn = class(TEntity);

type
  TAPOptionEnum = (apCoin, apFood, apRune, apGem, apBook, apPotion, apFlask,
    apScroll, apKey, apPlant, apFullscreen, apHdLevOfItem, apShPrice, apMagic);

type
  TGame = class(TObject)
  private
    FDifficulty: TDifficulty;
    FTimer: UInt;
    FWon: boolean;
    FCanClose: boolean;
    FShowMap: boolean;
    FShowID: boolean;
    FLCorpses: boolean;
    FScreenshot: string;
    FSpawn: TSpawn;
    FPortal: TSpawn;
    FPortalMap: TMapEnum;
    FPortalTile: TTileEnum;
    FShowEffects: boolean;
    FAPOption: array [TAPOptionEnum] of boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Difficulty: TDifficulty read FDifficulty write FDifficulty;
    property Timer: UInt read FTimer write FTimer;
    property Won: boolean read FWon write FWon;
    property CanClose: boolean read FCanClose write FCanClose;
    property ShowMap: boolean read FShowMap write FShowMap;
    property ShowID: boolean read FShowID write FShowID;
    property LCorpses: boolean read FLCorpses write FLCorpses;
    property Screenshot: string read FScreenshot write FScreenshot;
    property Spawn: TSpawn read FSpawn write FSpawn;
    property Portal: TSpawn read FPortal write FPortal;
    function GetStrDifficulty: string;
    function GetVersion: string;
    property PortalMap: TMapEnum read FPortalMap write FPortalMap;
    property PortalTile: TTileEnum read FPortalTile write FPortalTile;
    property ShowEffects: boolean read FShowEffects write FShowEffects;
    function GetTitle: string;
    procedure LoadConfig;
    procedure Start;
    procedure Log(S: string);
    function GetCursor: string;
    function IfThen(AValue: boolean; const ATrue: string;
      const AFalse: string): string;
    function EnsureRange(const AValue, AMax: Int): Int;
    function GetOption(I: TAPOptionEnum): boolean;
    procedure ChOption(I: TAPOptionEnum);
    procedure ChScreen;
  end;

var
  Game: TGame;

implementation

uses
  Math,
  Dialogs,
  BearLibTerminal,
  SysUtils,
  Trollhunter.Player,
  Trollhunter.UI.Log,
  Trollhunter.Scenes,
  Trollhunter.Item,
  Trollhunter.Mob,
  Trollhunter.Terminal,
  Trollhunter.Item.Shop,
  Trollhunter.Spellbook,
  Trollhunter.Helpers;

  { TGame }

procedure TGame.ChScreen;
begin
  if FAPOption[apFullscreen] then
    terminal_set('window.fullscreen=true')
  else
    terminal_set('window.fullscreen=false');
end;

constructor TGame.Create;
var
  I: UInt;
  J: TAPOptionEnum;
  IsUseLang: boolean;
begin
  Randomize;
  Timer := 0;
  Won := False;
  Mode.Game := False;
  Mode.Wizard := False;
  IsUseLang := False;
  for J := Low(TAPOptionEnum) to High(TAPOptionEnum) do
    FAPOption[J] := True;
  CanClose := False;
  ShowEffects := False;
  ShowMap := True;
  ShowID := False;
  LCorpses := True;
  Difficulty := dfNormal;
  FSpawn := TSpawn.Create;
  FPortal := TSpawn.Create;
  PortalMap := deDark_Wood;
  PortalTile := teStoneFloor;
  for I := 1 to ParamCount do
  begin
    if (LowerCase(ParamStr(I)) = '-w') then
      Mode.Wizard := True;
    if (LowerCase(ParamStr(I)) = '-l') then
      IsUseLang := True;
  end;
end;

destructor TGame.Destroy;
begin
  FreeAndNil(FPortal);
  FreeAndNil(FSpawn);
  inherited;
end;

function TGame.EnsureRange(const AValue, AMax: Int): Int;
begin
  Result := AValue.InRange(AMax);
end;

function TGame.IfThen(AValue: boolean; const ATrue: string;
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

function TGame.GetOption(I: TAPOptionEnum): boolean;
begin
  Result := FAPOption[I];
end;

function TGame.GetStrDifficulty: string;
begin
  case Difficulty of
    dfEasy:
      Result := 'Easy';
    dfNormal:
      Result := 'Normal';
    dfHard:
      Result := 'Hard';
    else
      Result := Terminal.Colorize('Hell', 'Red');
  end;

end;

function TGame.GetVersion: string;
begin
  Result := '0.14.1';
end;

procedure TGame.Log(S: string);
begin
  terminal_set('Log: ' + S);
end;

procedure TGame.LoadConfig;
begin
  // Settings
  FAPOption[apFullscreen] := terminal_get('window.fullscreen') = 'true';
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
  Player.Start;
  Player.Calc;
  Player.Fill;
  Spellbook.Start;
  Shops.New;
  // Intro
  MsgLog.Clear;
  MsgLog.Add(Terminal.Colorize(Format('%s %s %s',
    ['Welcome to Elvion!', 'You need to find and kill The King Troll!',
    'Press ? for help.']), clAlarm));
end;

function TGame.GetTitle: string;
begin
  Result := 'Trollhunter';
end;

initialization

  Game := TGame.Create;

finalization

  FreeAndNil(Game);

end.
