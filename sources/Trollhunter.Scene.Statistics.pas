unit Trollhunter.Scene.Statistics;

interface

uses uScenes,
  Trollhunter.Types;

type
  TSceneStatistics = class(TScene)
  public
    procedure Render; override;
    procedure Update(var Key: UInt); override;
  end;

implementation

uses SysUtils,
  BearLibTerminal,
  Trollhunter.UI,
  Trollhunter.Player,
  Trollhunter.Player.Types,
  Trollhunter.Game,
  Trollhunter.Language,
  Trollhunter.Player.Races,
  Trollhunter.Player.Classes,
  Trollhunter.Statistic,
  uAttribute,
  uBearLibItemsCommon,
  Trollhunter.Player.Skills,
  Trollhunter.Item,
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
    Title(_('Wizard Mode'));

    Add(_('Game Difficulty'), Game.GetStrDifficulty);
    Add(_('Monsters'), Ord(Length(MobBase)) - (13 + 7));
    Add(_('Bosses'), 13);
    Add(_('NPCs'), 7);
    Add(_('Items'), Ord(Length(ItemBase)));
    Add(_('Shops'), Shops.Count);
    Add(_('Quests'), Quests.Amount);
    Add(_('Talents'), Player.Talents.Amount);
    Add(_('Affixes'), Affixes.Amount);
    Add(_('Item Types'), Ord(High(TItemType)));
    Add(_('Skills'), Ord(High(TSkillEnum)));
  end
  else
  begin
    Title(Player.FullName);

    Add(_('Level'), Player.Attributes.Attrib[atLev].Value);
    Add(_('Scores'), Player.Statictics.Get(stScore));
    Add(_('Age'), Player.Statictics.Get(stAge));
    Add(_('Weight'), Player.Statictics.Get(stWeight));
    Add(_('Height'), Player.Statictics.Get(stHeight));
    Add(_('Metabolism'), Player.Statictics.Get(stMetabolism));

    Title(_('Statistics'), False);

    // Add(_('Talent'), Player.GetTalentName(Player.GetTalent(0)));
    Add(_('Tiles Moved'), Player.Statictics.Get(stTurn));
    Add(_('Monsters Killed'), Player.Statictics.Get(stKills));
    Add(_('Items Found'), Player.Statictics.Get(stFound));
    // Add(_('Chests Found'), );
    // Add(_('Doors Opened'), );
    Add(_('Potions Drunk'), Player.Statictics.Get(stPotDrunk));
    Add(_('Scrolls Read'), Player.Statictics.Get(stScrRead));
    Add(_('Spells Cast'), Player.Statictics.Get(stSpCast));
    Add(_('Foods Eaten'), Player.Statictics.Get(stFdEat));
    // Add(_('Melee Attack Performed'), );
    // Add(_('Ranged Attack Performed'), );
    // Add(_('Unarmed Attack Performed'), );
    // Add(_('Times Fallen Into Pit'), );
    // Add(_('Items Sold'), );
    Add(_('Items Used'), Player.Statictics.Get(stItUsed));
    Add(_('Items Repaired'), Player.Statictics.Get(stItRep));
    Add(_('Items Identified'), Player.Statictics.Get(stItIdent));
    Add(_('Items Crafted'), Player.Statictics.Get(stItCrafted));
    Add(_('Coins Looted'), Player.Statictics.Get(stCoinsLooted));
    // Add(_('Gold from Sales'), );
    // Add(_(''), );
  end;

  if Wizard then
  begin
    Title(_('Version'), False);

    Add(_('Game Version'), Game.GetVersion);
    Add(_('BeaRLibTerminal'), BearLibTerminal.terminal_get('version'));
    Self.Add();
    Add(_('BeaRLibItems'), Items_GetVersion);
  end;

  AddKey('Esc', _('Close'), not Mode.Wizard);
  if Mode.Wizard then
    if Wizard then
      AddKey('Z', _('Back'), True)
    else
      AddKey('Z', _('Wizard Mode'), True);
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
