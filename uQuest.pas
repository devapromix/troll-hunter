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
  TQuests = class(TObject)
    constructor Create;
    procedure Clear;
	function Count: Byte;
	procedure Add(const QuestEnum: TQuestEnum);
  end;

var
  Quests: TQuests;

implementation

uses SysUtils;

{ TQuests }

procedure TQuests.Add(const QuestEnum: TQuestEnum);
begin

end;

procedure TQuests.Clear;
begin

end;

function TQuests.Count: Byte;
begin
  Result := 0;
end;

constructor TQuests.Create;
begin

end;

initialization

Quests := TQuests.Create;

finalization

FreeAndNil(Quests);

end.
