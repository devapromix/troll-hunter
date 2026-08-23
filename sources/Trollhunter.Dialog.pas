unit Trollhunter.Dialog;

interface

uses
  Trollhunter.Types;

procedure GenRandomNPCWelcomeText();
function AskAbout(const ATopic: string): string;
function GetShopQuestion(const AShop: TShopEnum): string;
procedure PrepareNPCShopQuestions();

var
  NPCName: string = '';
  NPCShops: set of TShopEnum = [];
  NPCActions: set of TNPCActionEnum = [];
  NPCShopQuestion: array [TShopEnum] of string;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.UI.Log;

procedure GenRandomNPCWelcomeText();
const
  CNPCTexts: array [0 .. 29] of string = (
    'What can I do for you?',
    'What can I get you today?',
    'Good day!',
    'Welcome, traveler.',
    'Looking for supplies?',
    'Take a look at my wares.',
    'Need something?',
    'The best goods in town.',
    'Everything has a price.',
    'Have a look around.',
    'Welcome to my shop.',
    'How may I help you?',
    'Care to trade?',
    'Fresh goods for sale!',
    'See anything you like?',
    'Quality goods, fair prices.',
    'Spend your gold wisely.',
    'You won''t find better deals.',
    'Feel free to browse.',
    'May fortune favor you.',
    'Welcome! Take your time.',
    'Looking for the finest gear?',
    'I have just what you need.',
    'Every item has been carefully selected.',
    'Looking for weapons or supplies?',
    'A smart adventurer is always well equipped.',
    'Take a look. You might find a bargain.',
    'If I don''t have it, nobody does.',
    'Only the finest goods for my customers.',
    'You''ve come to the right place.'
  );
begin
  MsgLog.Add(Format('%s says: "%s"',
    [NPCName, CNPCTexts[Math.RandomRange(0, Length(CNPCTexts))]]));
end;

function AskAbout(const ATopic: string): string;
const
  CTemplates: array [0 .. 3] of string = (
    'What %s do you have?',
    'Show me your %s',
    'Got any %s?',
    'What %s do you sell?'
  );
var
  LIndex: Integer;
begin
  LIndex := Math.RandomRange(0, Length(CTemplates));
  Result := Format(CTemplates[LIndex], [ATopic]);
end;

function GetShopQuestion(const AShop: TShopEnum): string;
const
  CShopTopics: array [TShopEnum] of string = (
    'potions', 'scrolls', 'healing items', 'mana potions', '', 'armor',
    'gloves', 'food', 'weapons', 'boots', '', 'shields', 'helmets',
    'jewelry', 'gems', 'runes', 'quivers', 'staves', 'wands', 'books',
    'bows', 'daggers', 'venoms'
  );
begin
  case AShop of
    shSmith:
      Result := 'What do you have for sale?';
    shTavern:
      Result := 'What''s on the menu?';
  else
    Result := AskAbout(CShopTopics[AShop]);
  end;
end;

procedure PrepareNPCShopQuestions();
var
  LShop: TShopEnum;
begin
  for LShop := Low(TShopEnum) to High(TShopEnum) do
    if (LShop in NPCShops) then
      NPCShopQuestion[LShop] := GetShopQuestion(LShop);
end;

end.
