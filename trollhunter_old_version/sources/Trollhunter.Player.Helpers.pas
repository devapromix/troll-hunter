unit Trollhunter.Player.Helpers;

interface

uses
  Trollhunter.Player;

type
  TPlayerHelper = class helper for TPlayer
    function FullName: string;
    function SaveName: string;
    function Gender: string;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Trollhunter.Game,
  Trollhunter.Player.Types,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.Language,
  Trollhunter.Attribute,
  Trollhunter.Utils;

{ TPlayerHelper }

function TPlayerHelper.FullName: string;
begin
  Result := Format('%s, %s (%s), %s', [IfThen(Player.Name.Trim = '', _('PLAYER'), Player.Name), _(Races.GetName(Player.HRace)), Gender,
    _(PCClasses.GetName(Player.HClass))]);
end;

function TPlayerHelper.Gender: string;
begin
  if (Player.Sex = sxMale) then
    Result := _('Male')
  else
    Result := _('Female');
end;

function TPlayerHelper.SaveName: string;
begin
  Result := Format('%s, %s, %s, %d level [[%s]]', [IfThen(Player.Name.Trim = '', _('PLAYER'), Player.Name), _(Races.GetName(Player.HRace)),
    _(PCClasses.GetName(Player.HClass)), Player.Attributes.Attrib[atLev].Value, Utils.GetDateTime]);
end;

end.
