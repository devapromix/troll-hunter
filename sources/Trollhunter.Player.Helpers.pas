unit Trollhunter.Player.Helpers;

interface

uses Trollhunter.Player;

type
  TPlayerHelper = class helper for TPlayer
    function FullName: string;
    function Gender: string;
  end;

implementation

uses SysUtils,
  Trollhunter.Game,
  Trollhunter.Player.Types,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.Language;

{ TPlayerHelper }

function TPlayerHelper.FullName: string;
begin
  Result := Format('%s, %s (%s), %s', [Game.IfThen(Trim(Player.Name) = '',
    _('PLAYER'), Player.Name), Races.GetName(Player.HRace), Gender,
    Classes.GetName(Player.HClass)])
end;

function TPlayerHelper.Gender: string;
begin
  if (Player.Sex = sxMale) then
    Result := _('Male')
  else
    Result := _('Female');
end;

end.
