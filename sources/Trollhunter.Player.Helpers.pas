unit Trollhunter.Player.Helpers;

interface

uses
  Trollhunter.Types,
  Trollhunter.Player;

type

  { TPlayerHelper }

  TPlayerHelper = class helper for TPlayer
    function FullName: string;
    function GenderStr: string;
    function Satiation: string;
    function StartGold: UInt;
    function HasSpellbook: boolean;
    function HasQuiver: boolean;
  end;

implementation

uses
  SysUtils,
  Trollhunter.Game,
  Trollhunter.Terminal,
  Trollhunter.Attribute,
  Trollhunter.Player.Types,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes;

  { TPlayerHelper }

function TPlayerHelper.FullName: string;
begin
  Result := Format('%s, %s (%s), %s', [Player.Name, Races.GetName(Player.HRace),
    Player.GenderStr, Trollhunter.Player.Classes.Classes.GetName(Player.HClass)]);
end;

function TPlayerHelper.GenderStr: string;
begin
  if (Player.Gender = gdMale) then
    Result := 'Male'
  else
    Result := 'Female';
end;

function TPlayerHelper.Satiation: string;
begin
  case Attributes.Attrib[atSat].Value of
    0 .. StarvingMax:
      Result := 'Starving';
    StarvingMax + 1 .. 1500:
      Result := 'Near starving';
    1501 .. 2000:
      Result := 'Very hungry';
    2001 .. 2500:
      Result := 'Hungry';
    SatiatedMax + 1 .. 10000:
      Result := 'Full';
    10001 .. 11000:
      Result := 'Very full';
    11001 .. EngorgedMax:
      Result := 'Engorged';
    else
      Result := '';
  end;
  case Attributes.Attrib[atSat].Value of
    0 .. StarvingMax:
      Result := Terminal.Colorize(Result, 'Light Red');
    StarvingMax + 1 .. SatiatedMax:
      Result := Terminal.Colorize(Result, 'Light Yellow');
    else
      Result := Terminal.Colorize(Result, 'Light Green');
  end;

end;

function TPlayerHelper.StartGold: UInt;
const
  CGold: array [TDifficulty] of UInt = (400, 250, 100, 50);
begin
  Result := CGold[Game.Difficulty];
end;

function TPlayerHelper.HasSpellbook: boolean;
begin
  Result := Self.GetEquippedIndex(stSpellbook) >= 0;
end;

function TPlayerHelper.HasQuiver: boolean;
begin
  Result := (Self.GetQuiverIndex >= 0);
end;

end.
