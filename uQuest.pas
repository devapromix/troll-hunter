unit uQuest;

interface

uses uEntity, uMob;

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
  TQuestBase = record
    Level: Byte;
    QuestType: TQuestType;
	Mobs: TMobEnum;
	Amount: Byte;
  end;

type
  TQuestEnum = (qsKill);

const
  QuestBase: array [TQuestEnum] of TQuestBase = (
  // Kill N mobs
  (Level: 1; QuestType: qtKillMobs; Mobs: mbBlack_Bear; Amount: 3;)
  );

type
  TQuests = class(TObject)
    constructor Create;
    procedure Clear;
  end;

var
  Quests: TQuests;

implementation

uses SysUtils;

{ TQuests }

procedure TQuests.Clear;
begin

end;

constructor TQuests.Create;
begin

end;

initialization

Quests := TQuests.Create;

finalization

FreeAndNil(Quests);

end.
