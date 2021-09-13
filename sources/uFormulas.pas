unit uFormulas;

interface

function GetWizardEyePower: Integer;
function GetAdvMana(A: Integer): Integer;
function GetMaxLife(A: Integer): Integer;
function GetMaxMana(A: Integer): Integer;
function GetMaxAP(A: Integer): Integer;
function GetMaxCount(A: Integer): Integer;
function GetMaxWeight(A: Integer): Integer;

implementation

uses uCreatures, uSkill;

// Сила заклинания "Глаз Чародея". Pадиус.
function GetWizardEyePower: Integer;
begin
  Result := Creatures.PC.Skill.GetSkill(skMagic, True) + 5;
end;

// Доп. мана от навыка магия.
function GetAdvMana(A: Integer): Integer;
begin
  Result := Round(A * 1.7);
end;

// Макс. жизнь.
function GetMaxLife(A: Integer): Integer;
begin
  Result := Round(A * 2.3);
end;

// Макс. мана.
function GetMaxMana(A: Integer): Integer;
begin
  Result := Round(A * 4.4);
end;

// Макс. оч. действий.
function GetMaxAP(A: Integer): Integer;
begin
  Result := Round(25 - A / 1.4);
  if (Result < 1) then Result := 1;
end;

// Макс. возможный объем инвентаря.
function GetMaxCount(A: Integer): Integer;
begin
  Result := Round(A / 1.3);
end;

// Макс. возможный вес инвентаря.
function GetMaxWeight(A: Integer): Integer;
begin
  Result := Round(A * 2.1);
end;

end.
