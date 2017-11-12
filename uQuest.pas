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
    Level: Byte;
    QuestType: TQuestType;
	  Mobs: TSetOfMobEnum;
	  Amount: TMinMax;
  end;

type
  TQuestEnum = (qsKillNBears);

const
  QuestBase: array [TQuestEnum] of TQuestBase = (
  // Kill N bears
  (Level: 1; QuestType: qtKillMobs; Mobs: [mbBlack_Bear, mbGrizzly_Bear]; Amount: (Min: 3; Max: 5;);)
  );

type
  TQuest = record
    Level: Byte;
    QuestType: TQuestType;
	  Mob: TMobEnum;
	  Amount: Byte;
  end;

type
  TQuests = class(TObject)
  private
    FQuest: array of TQuest;
  public
    constructor Create;
    procedure Clear;
	  function Count: Byte;
	  procedure Add(const QuestEnum: TQuestEnum);
  end;

var
  Quests: TQuests;

implementation

uses SysUtils, Dialogs, uMap;

{ TQuests }

procedure TQuests.Add(const QuestEnum: TQuestEnum);
begin
  SetLength(FQuest, Count() + 1);
  with FQuest[Count() - 1] do
  begin
    Level := 1;
    QuestType := qtKillMobs;
	  Mob := mbBlack_Bear;
	  Amount := 3;
  end;
  Mobs.AddGroup(deDarkWood, mbBlack_Bear, 3);
  
  //case QuestEnum of

  //end;
  ShowMessage('Quests.Add');
end;

procedure TQuests.Clear;
begin
  SetLength(FQuest, 0);
end;

function TQuests.Count: Byte;
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
