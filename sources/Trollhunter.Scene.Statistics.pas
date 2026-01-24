unit Trollhunter.Scene.Statistics;

interface

uses
  uScenes,
  Trollhunter.Types;

type
  TSceneStatistics = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses
  SysUtils,
  Trollhunter.UI,
  Trollhunter.Player,
  Trollhunter.Player.Types,
  Trollhunter.Game,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.Statistic,
  uAttribute,
  uBearLibItemsCommon,
  BearLibTerminal,
  Trollhunter.Skill,
  uItem,
  uMob,
  Trollhunter.Item.Affixes,
  Trollhunter.Item.Types,
  Trollhunter.Item.Shop,
  uQuest,
  Trollhunter.Player.Helpers;

var
  Wizard: Boolean = False;

  { TSceneStatistics }

procedure TSceneStatistics.Render;
begin
  Y := 1;
  if Mode.Wizard and Wizard then
  begin
    Title('Wizard Mode');

    Add('Monsters', Ord(Length(MobBase)) - (13 + 7));
    Add('Bosses', 13);
    Add('NPCs', 7);
    Add('Items', Ord(Length(ItemBase)));
    Add('Shops', Shops.Count);
    Add('Quests', Quests.Amount);
    Add('Talents', Player.Talents.Amount);
    Add('Affixes', Affixes.Amount);
    Add('Item Types', Ord(High(TItemType)));
    Add('Skills', Ord(High(TSkillEnum)));
  end
  else
  begin
    Title(Player.FullName);

    Add('Level', Player.Attributes.Attrib[atLev].Value);
    Add('Scores', Player.Statictics.Get(stScore));
    Add('Age', Player.Statictics.Get(stAge));
    Add('Weight', Player.Statictics.Get(stWeight));
    Add('Height', Player.Statictics.Get(stHeight));
    Add('Metabolism', Player.Statictics.Get(stMetabolism));

    Title('Statistics', False);

    // Add(_('Talent'), Player.GetTalentName(Player.GetTalent(0)));
    Add('Tiles Moved', Player.Statictics.Get(stTurn));
    Add('Monsters Killed', Player.Statictics.Get(stKills));
    Add('Items Found', Player.Statictics.Get(stFound));
    // Add(_('Chests Found'), );
    // Add(_('Doors Opened'), );
    Add('Potions Drunk', Player.Statictics.Get(stPotDrunk));
    Add('Scrolls Read', Player.Statictics.Get(stScrRead));
    Add('Spells Cast', Player.Statictics.Get(stSpCast));
    Add('Foods Eaten', Player.Statictics.Get(stFdEat));
    // Add(_('Melee Attack Performed'), );
    // Add(_('Ranged Attack Performed'), );
    // Add(_('Unarmed Attack Performed'), );
    // Add(_('Times Fallen Into Pit'), );
    // Add(_('Items Sold'), );
    Add('Items Used', Player.Statictics.Get(stItUsed));
    Add('Items Repaired', Player.Statictics.Get(stItRep));
    Add('Items Identified', Player.Statictics.Get(stItIdent));
    Add('Items Crafted', Player.Statictics.Get(stItCrafted));
    Add('Coins Looted', Player.Statictics.Get(stCoinsLooted));
    // Add(_('Gold from Sales'), );
    // Add(_(''), );
  end;

  if Wizard then
  begin
    Title('Version', False);

    Add('Game Version', Game.GetVersion);
    Add('BeaRLibTerminal', BearLibTerminal.terminal_get('version'));
    Self.Add();
    Add('BeaRLibItems', Items_GetVersion);
  end;

  AddKey('Esc', 'Close', not Mode.Wizard);
  if Mode.Wizard then
    if Wizard then
      AddKey('Z', 'Back', True)
    else
      AddKey('Z', 'Wizard Mode', True);
end;

procedure TSceneStatistics.Update(var Key: UInt);
begin
  case Key of
    TK_ESCAPE:
      // Close
      Scenes.SetScene(scGame);
    TK_Z:
      if Mode.Wizard then
        Wizard := not Wizard;
  end;
end;

end.
