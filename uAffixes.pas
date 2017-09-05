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
    Price: Word;
    Occurence: set of TItemType;
    Durability: Word;
    Defense: TMinMax;
    Damage: TBaseDamage;
  end;

type
  TSuffixEnum = (aNone, aDefense, aDefense2);

const
  SuffixBase: array [TSuffixEnum] of TSuffixBase = (
    // None
    (),
    // of Defense
    (Level: 1; Price: 200; Defense: (Min: 10; Max: 50);),
    // of Defense
    (Level: 1; Price: 300; Defense: (Min: 60; Max: 100);));

type
  TAffixes = class
  public
    function GetSuffixName(SuffixEnum: TSuffixEnum): string;
    procedure DoSuffix(var AItem: Item);
  end;

var
  Affixes: TAffixes = nil;

implementation

uses SysUtils, Math, uTerminal, GNUGetText;

procedure TAffixes.DoSuffix(var AItem: Item);
var
  SB: TSuffixBase;
begin
  SB := SuffixBase[TSuffixEnum(AItem.Identify)];
  case TSuffixEnum(AItem.Identify) of
    aDefense, aDefense2:
      begin
        if (SB.Defense.Min > 0) then
          AItem.Defense := AItem.Defense +
            Math.EnsureRange(Math.RandomRange(SB.Defense.Min,
            SB.Defense.Max + 1), 1, High(Byte));
      end;
  end;
end;

function TAffixes.GetSuffixName(SuffixEnum: TSuffixEnum): string;
begin
  case SuffixEnum of
    aDefense:
      Result := ' of Defense I';
    aDefense2:
      Result := ' of Defense II';
  else
    Result := '';
  end;
end;

initialization

Affixes := TAffixes.Create;

finalization

FreeAndNil(Affixes);

end.
