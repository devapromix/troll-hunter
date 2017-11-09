unit uQuest;

interface

type
  TQuestBase = record

  end;

type
  TQuestEnum = ();

const
  QuestBase: array [TQuestEnum] of TQuestBase = ();

type
  TQuests = class(TObject);

var
  Quests: TQuests;

implementation

uses SysUtils;

initialization

Quests := TQuests.Create;

finalization

FreeAndNil(Quests);

end.
