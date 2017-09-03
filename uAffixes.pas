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

function GetSuffixName(AItem: Item): string;

implementation

uses uTerminal, GNUGetText;

function GetSuffixName(AItem: Item): string;
begin
  Result := '';
  case AItem.Identify of
    0: Result := Terminal.Colorize(' [[' + _('Undefined') + ']]', 'Red');
    1..99:
      Result := ' of ...'
  end;
end;

end.
