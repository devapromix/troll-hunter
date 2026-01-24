unit Trollhunter.Player.Helpers;

interface

uses
  Trollhunter.Player;

type
  TPlayerHelper = class helper for TPlayer
    function FullName: string;
    function Gender: string;
  end;

implementation

uses
  SysUtils,
  Trollhunter.Player.Types,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes;

{ TPlayerHelper }

function TPlayerHelper.FullName: string;
begin
  Result := Format('%s, %s (%s), %s', [Player.Name, Races.GetName(Player.HRace),
    Gender, Trollhunter.Player.Classes.Classes.GetName(Player.HClass)])
end;

function TPlayerHelper.Gender: string;
begin
  if (Player.Sex = sxMale) then
    Result := 'Male'
  else
    Result := 'Female';
end;

end.
