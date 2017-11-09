unit uQuest;

interface

{ TODO -cУбить существо : Убить уникального босса на локации (от первой до предпоследней). }
{ TODO -cУбить существо : Убить Н существ на локации (от первой до последней). }
{ TODO -cПринести предмет : Собрать Н предметов с существ на локации (от первой до последней). }
{ TODO -cПринести предмет : Найти уникальный предмет на локации (от первой до последней). }
{ TODO -cПосетить локацию : Посетить определенную локацию (от первой до последней). }
{ TODO -cПоговорить с НПЦ : Поговорить с определенным НПЦ (от первой до последней). }
{ TODO -cТаймер : Доставить предмет определенному НПЦ в указанной локации за отведенный отрезок времени (от первой до последней). }
{ TODO -cТаймер : Убить Н существ в указанной локации за отведенный отрезок времени (от первой до последней). }

type
  TQuestBase = record

  end;

type
  TQuestEnum = ();

const
  QuestBase: array [TQuestEnum] of TQuestBase = ();

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
