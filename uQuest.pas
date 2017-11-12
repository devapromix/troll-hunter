unit uQuest;

interface

uses uCreature, uMob;

{ TODO -cУбить существо : Убить уникального босса на локации (от первой до предпоследней). }
{ TODO -cУбить существо : Убить N существ на локации (от первой до последней). }
{ TODO -cПринести предмет : Собрать N предметов с существ на локации (от первой до последней). }
{ TODO -cПринести предмет : Найти уникальный предмет на локации (от первой до последней). }
{ TODO -cПосетить локацию : Посетить определенную локацию (от первой до последней). }
{ TODO -cПоговорить с НПЦ : Поговорить с определенным НПЦ (от первой до последней). }
{ TODO -cТаймер : Доставить предмет определенному НПЦ в указанной локации за отведенный отрезок времени (от первой до последней). }
{ TODO -cТаймер : Убить N существ в указанной локации за отведенный отрезок времени (от первой до последней). }

type
  TQuestType = (qtKillMobs);
  
type
  TSetOfMobEnum = set of TMobEnum;

type
  TQuestBase = record
    Level: Integer;
    QuestType: TQuestType;
	  Mobs: TSetOfMobEnum;
	  Amount: TMinMax;
  end;

type
  TQuestEnum = (qeKillNBears);

const
  QuestBase: array [TQuestEnum] of TQuestBase = (
  // Kill N bears
  (Level: 1; QuestType: qtKillMobs; Mobs: [mbBlack_Bear, mbGrizzly_Bear]; Amount: (Min: 3; Max: 5;);)
  );

type
  TQuestState = (qsActive, qsDone, qsFinish);

type
  TQuest = record
    Level: Integer;
    QuestState: TQuestState;
    QuestType: TQuestType;
	  Mob: TMobEnum;
	  Amount: Integer;
    Kills: Integer;
  end;

type
  TQuests = class(TObject)
  private
    FQuest: array of TQuest;
    function GetQuest(I: Integer): TQuest;
    procedure SetQuest(I: Integer; const Value: TQuest);
  public
    procedure Clear;
    constructor Create;
	  function Count: Integer;
	  procedure Add(const QuestEnum: TQuestEnum);
    property Quest[I: Integer]: TQuest read GetQuest write SetQuest;
    procedure DoQuest(const AQuestType: TQuestType; const Value: Integer);
  end;

var
  Quests: TQuests;

implementation

uses SysUtils, Dialogs, uMap;

{ TQuests }

function TQuests.GetQuest(I: Integer): TQuest;
begin
  Result := FQuest[I]
end;

procedure TQuests.SetQuest(I: Integer; const Value: TQuest);
begin
  FQuest[I] := Value
end;

procedure TQuests.DoQuest(const AQuestType: TQuestType; const Value: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    case AQuestType of
      qtKillMobs:
        with FQuest[I] do
          if ((QuestType = qtKillMobs) and (QuestState = qsActive) and (Mob = TMobEnum(Value))) then
            Kills := Kills + 1;
    end;
end;

procedure TQuests.Add(const QuestEnum: TQuestEnum);
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
  Mobs.AddGroup(deDarkWood, mbBlack_Bear, 3);
//  ShowMessage('Quests.Add');
end;

procedure TQuests.Clear;
begin
  SetLength(FQuest, 0);
end;

function TQuests.Count: Integer;
begin
  Result := Length(FQuest);
end;

constructor TQuests.Create;
begin
  Clear;
end;

initialization

Quests := TQuests.Create;

finalization

FreeAndNil(Quests);

end.
