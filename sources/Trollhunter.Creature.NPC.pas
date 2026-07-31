unit Trollhunter.Creature.NPC;

interface

uses
  Trollhunter.Mob;

procedure GenRandomNPCWelcomeText();

var
  NPCName: string = '';
  NPCType: set of TNPCType = [];

implementation

uses
  Math,
  SysUtils,
  Trollhunter.UI.Log,
  Trollhunter.Scenes;

procedure GenRandomNPCWelcomeText();
const
  NPCTexts: array[0..29] of string = (
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
    [NPCName, NPCTexts[Math.RandomRange(0, Length(NPCTexts))]]));
end;

end.
