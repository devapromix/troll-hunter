unit Trollhunter.Game;

interface

uses
  Trollhunter.Scores;

type
  TPCInfo = record
    Level: Byte;
    Rating: Cardinal;
    Dungeon: Byte;
  end;

  TGame = class(TObject)
  private
    FScores: TScores;
    procedure SetScores(const Value: TScores);
  public
    procedure New;
    procedure Save;
    procedure Load(I: Integer = -1);
    function GetPCInfo(AFileName: string): TPCInfo;
    constructor Create;
    destructor Destroy; override;
    property Scores: TScores read FScores write SetScores;
  end;

var
  Game: TGame;

implementation

uses
  SysUtils,
  Trollhunter.MainForm,
  Trollhunter.Map,
  Trollhunter.Utils,
  Trollhunter.Creatures,
  Trollhunter.Log,
  Trollhunter.Zip,
  Trollhunter.Graph,
  Trollhunter.Error,
  Trollhunter.TempSys,
  Trollhunter.Item,
  Trollhunter.Settings,
  Trollhunter.Resources;

{ TGame }

constructor TGame.Create;
begin
  Scores := TScores.Create(26);
  if FileExists(Path + 'save\Scores.rec') then
    Scores.Load;
end;

destructor TGame.Destroy;
begin
  Scores.Free;
  inherited;
end;

procedure TGame.New;
begin
  try
    Creatures.PC.Create;
    Game.Load;
    Log.Clear;
  except
    on E: Exception do
      Error.Add('Game.New', E.Message);
  end;
end;

procedure TGame.Load(I: Integer = -1);
var
  AFileName: string;
  Z: TZip;
begin
  IsGame := True;
  with Creatures do
    try
      AFileName := PC.Name;
      if not FileExists(Path + 'save\' + AFileName + '.sav') then
      begin
        PC.World.Gen;
        Map.Gen(PC.Dungeon);
        PC.Redraw;
        Res.Free;
        Res := TResources.Create;
        Exit;
      end;
      // Open save
      Z := TZip.Create(MainForm);
      try
        Z.Password := PWD;
        Z.FileName := Path + 'save\' + AFileName + '.sav';
        Z.OpenArchive;
        // PC
        PC.Text := Z.ExtractToText('pc.txt');
        // Scrolls Properties
        PC.Scrolls.Text := Z.ExtractToText('scrolls.txt');
        // Potions Properties
        PC.Potions.Text := Z.ExtractToText('potions.txt');
        // PC.Inv
        PC.Inv.Text := Z.ExtractToText('inv.txt');
        if (I > -1) then
          PC.Dungeon := I;
        // PC.Skill
        PC.Skill.Text := Z.ExtractToText('skill.txt');
        // Map
        if not Z.FileExists(IntToStr(PC.Dungeon) + '.m') then
        begin
          Map.Gen(PC.Dungeon);
        end
        else
        begin
          Map.FM.Text := Z.ExtractToText(IntToStr(PC.Dungeon) + '.m');
          Map.FV.Text := Z.ExtractToText(IntToStr(PC.Dungeon) + '.v');
          Map.FS.Text := Z.ExtractToText(IntToStr(PC.Dungeon) + '.s');
          Map.FD.Text := Z.ExtractToText(IntToStr(PC.Dungeon) + '.d');
          Map.FL.Text := Z.ExtractToText(IntToStr(PC.Dungeon) + '.l');
          Map.Load(PC.Dungeon);
        end;
        // Counters
        PC.TempSys.Text := Z.ExtractToText('effects.txt');
        // Statistics
        PC.Statistics.Text := Z.ExtractToText('statistics.txt');
        // Log
        Log.Text := Z.ExtractToText('log.txt');
        // World
        PC.World.Text := Z.ExtractToText('world.p');
        //
        Z.CloseArchive;
      finally
        Z.Free;
      end;
      PC.Redraw;
      Res.Free;
      Res := TResources.Create;
    except
      on E: Exception do
        Error.Add('Game.Load', E.Message);
    end;
end;

function TGame.GetPCInfo(AFileName: string): TPCInfo;
var
  Z: TZip;
begin
  Z := TZip.Create(MainForm);
  try
    Z.Password := PWD;
    Z.FileName := AFileName;
    Z.OpenArchive;
    // PC
    Creatures.PC.Text := Z.ExtractToText('pc.txt');
    Result.Level := Creatures.PC.Prop.Level;
    Result.Rating := Creatures.PC.Rating;
    Result.Dungeon := Creatures.PC.Dungeon;
    Z.CloseArchive;
  finally
    Z.Free;
  end;
end;

procedure TGame.Save;
var
  AFileName: string;
  S: TSettings;
  Z: TZip;
begin
  if not IsGame then
    Exit;
  with Creatures do
    try
      if not PC.Life.IsMin and (PC.Name <> '') then
      begin
        AFileName := PC.Name;
        Z := TZip.Create(MainForm);
        try
          Z.Password := PWD;
          Z.FileName := Path + 'save\' + AFileName + '.sav';
          Z.OpenArchive;
          // PC
          Z.AddFromString('pc.txt', PC.Text);
          // Scrolls Properties
          Z.AddFromString('scrolls.txt', PC.Scrolls.Text);
          // Potions Properties
          Z.AddFromString('potions.txt', PC.Potions.Text);
          // PC.Inv
          Z.AddFromString('inv.txt', PC.Inv.Text);
          // PC.Skill
          Z.AddFromString('skill.txt', PC.Skill.Text);
          // Map
          Map.Save(PC.Dungeon);
          Z.AddFromString(IntToStr(PC.Dungeon) + '.m', Map.FM.Text);
          Z.AddFromString(IntToStr(PC.Dungeon) + '.v', Map.FV.Text);
          Z.AddFromString(IntToStr(PC.Dungeon) + '.s', Map.FS.Text);
          Z.AddFromString(IntToStr(PC.Dungeon) + '.d', Map.FD.Text);
          Z.AddFromString(IntToStr(PC.Dungeon) + '.l', Map.FL.Text);
          // Counters
          Z.AddFromString('effects.txt', PC.TempSys.Text);
          // Statistics
          Z.AddFromString('statistics.txt', PC.Statistics.Text);
          // Log
          Z.AddFromString('log.txt', Log.Text);
          // World
          Z.AddFromString('world.p', PC.World.Text);
          //
          Z.CloseArchive;
        finally
          Z.Free;
        end;
        S := TSettings.Create;
        try
          S.Write('Settings', 'LastName', PC.Name);
        finally
          S.Free;
        end;
      end;
    except
      on E: Exception do
        Error.Add('Game.Save', E.Message);
    end;
end;

procedure TGame.SetScores(const Value: TScores);
begin
  FScores := Value;
end;

initialization

Game := TGame.Create;

finalization

Game.Free;

end.
