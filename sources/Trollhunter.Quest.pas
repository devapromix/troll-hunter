unit Trollhunter.Quest;

interface

uses
  Trollhunter.Types,
  Trollhunter.Creature,
  Trollhunter.Mob;

{ TODO -cKill Creature : Kill the unique boss in a location (from the first to the penultimate). }
{ TODO -cKill Creature : Kill N creatures in a location (from the first to the last). }
{ TODO -cCollect Item : Collect N items from creatures in a location (from the first to the last). }
{ TODO -cFind Item : Find a unique item in a location (from the first to the last). }
{ TODO -cVisit Location : Visit a specific location (from the first to the last). }
{ TODO -cTalk to NPC : Talk to a specific NPC (from the first to the last). }
{ TODO -cTimed Quest : Deliver an item to a specific NPC in the specified location within the time limit (from the first to the last). }
{ TODO -cTimed Quest : Kill N creatures in the specified location within the time limit (from the first to the last). }

type
  TQuestType = (qtKillMobs);

type
  TSetOfMobEnum = set of TMobEnum;

type
  TQuestBase = record
    Level: Int;
    QuestType: TQuestType;
    Mobs: TSetOfMobEnum;
    Amount: TMinMax;
  end;

type
  TQuestEnum = (qeKillNBears);

const
  QuestBase: array [TQuestEnum] of TQuestBase = (
    // The Hunt (Kill N creatures)
    (Level: 1; QuestType: qtKillMobs; Mobs: [mbBlack_Bear, mbGrizzly_Bear];
    Amount: (Min: 3; Max: 5;);));

type
  TQuestState = (qsActive, qsDone, qsFinish);

type
  TQuest = record
    Level: Int;
    QuestState: TQuestState;
    QuestType: TQuestType;
    Mob: TMobEnum;
    Amount: Int;
    Kills: Int;
  end;

type
  TQuests = class(TObject)
  private
    FCurrent: TQuestEnum;
    FQuest: array of TQuest;
    function GetQuest(I: Int): TQuest;
    procedure SetQuest(I: Int; const Value: TQuest);
  public
    procedure Clear();
    constructor Create();
    function Count(): Int;
    function Amount(): Int;
    procedure Add(const AQuestEnum: TQuestEnum);
    property Quest[I: Int]: TQuest read GetQuest write SetQuest;
    procedure DoQuest(const AQuestType: TQuestType; const Value: Int);
    property Current: TQuestEnum read FCurrent write FCurrent;
    function GetName(const AQuestEnum: TQuestEnum): string;
  end;

var
  Quests: TQuests;

implementation

uses
  SysUtils,
  Trollhunter.Map,
  Trollhunter.UI.Log;

{ TQuests }

function TQuests.GetName(const AQuestEnum: TQuestEnum): string;
begin
  Result := 'The Hunt';
end;

function TQuests.GetQuest(I: Int): TQuest;
begin
  Result := FQuest[I]
end;

procedure TQuests.SetQuest(I: Int; const Value: TQuest);
begin
  FQuest[I] := Value
end;

procedure TQuests.DoQuest(const AQuestType: TQuestType; const Value: Int);
var
  I: Int;
begin
  for I := 0 to Count() - 1 do
    with FQuest[I] do
    begin
      // Killing monsters
      if ((QuestType = AQuestType) and (QuestState = qsActive) and
        (Mob = TMobEnum(Value))) then
        Kills := Kills + 1;
    end;
end;

procedure TQuests.Add(const AQuestEnum: TQuestEnum);
begin
  if True then
  begin
    SetLength(FQuest, Count() + 1);
    with FQuest[Count() - 1] do
    begin
      Level := 1;
      QuestState := qsActive;
      QuestType := qtKillMobs;
      Mob := mbBlack_Bear;
      Amount := 3;
      // Counters
      Kills := 0;
    end;
    Mobs.AddGroup(deDark_Wood, mbBlack_Bear, 3);
    MsgLog.Add('The new quest is added to the log.');
  end;
end;

function TQuests.Amount: Int;
begin
  Result := Ord(High(TQuestEnum)) + 1;
end;

procedure TQuests.Clear();
begin
  SetLength(FQuest, 0);
end;

function TQuests.Count(): Int;
begin
  Result := Length(FQuest);
end;

constructor TQuests.Create();
begin
  Clear();
end;

initialization

Quests := TQuests.Create();

finalization

FreeAndNil(Quests);

end.
