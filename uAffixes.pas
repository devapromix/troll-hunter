unit uAffixes;

interface

uses uEntity, uItem, BeaRLibItems;

// Prefix:
// Minotaur, Ogre, Trollish, Dragon (Str),
// Elven, Lizardman (Dex),
// Suffix:
// of Defense

type
  TSuffixBase = record
    Level: Byte;
    Color: Cardinal;
    Price: TMinMax;
    Occurence: set of TItemType;
    Durability: Word;
    Defense: TMinMax;
    Damage: TBaseDamage;
  end;

type
  TSuffixEnum = (aDefense);

const
  SuffixBase: array [TSuffixEnum] of TSuffixBase = (
    // of Defense
    (Level: 1;));

type
  TAffixes = class
  public
    function GetSuffixName(SuffixEnum: TSuffixEnum): string;
  end;

var
  Affixes: TAffixes = nil;

implementation

uses SysUtils, uTerminal, GNUGetText;

function TAffixes.GetSuffixName(SuffixEnum: TSuffixEnum): string;
begin
  case SuffixEnum of
    aDefense:
      Result := ' of Defense';
  else
    Result := '';
  end;
end;

initialization

Affixes := TAffixes.Create;

finalization

FreeAndNil(Affixes);

end.
