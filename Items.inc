type
  TItemEnum = (
    // All
    iGold, iMinHPot, iMinMPot,
    // Dark Wood
    iShortSword,
    // Gray Cave
    iItemA,
    // Deep Cave
    iItemB,
    // Blood Cave
    iItemC,
    // Dungeon of Doom
    iItemD
  );  

const
  ItemBase: array [TItemEnum] of TItemBase = (
    // All
    (Symbol: '$'; Name: 'Gold';                ItemType: itCoin;   MaxStack: 1000; MaxDurability: 0;  Color: clYellow;   Deep: deDarkWood;),
    (Symbol: '!'; Name: 'Minor Health Potion'; ItemType: itPotion; MaxStack:   10; MaxDurability: 0;  Color: clRed;      Deep: deDarkWood;),
    (Symbol: '!'; Name: 'Minor Mana Potion';   ItemType: itPotion; MaxStack:   10; MaxDurability: 0;  Color: clBlue;     Deep: deDarkWood;),
    // Dark Wood
    (Symbol: '/'; Name: 'Short Sword';         ItemType: itSword;  MaxStack: 1;    MaxDurability: 30; Color: clWhite;    Deep: deDarkWood;),
    // Gray Cave
    (Symbol: '\'; Name: 'Item A';              ItemType: itSword;  MaxStack: 1;    MaxDurability: 40; Color: clYellow;   Deep: deGrayCave;),
    // Deep Cave
    (Symbol: '['; Name: 'Item B';              ItemType: itSword;  MaxStack: 1;    MaxDurability: 50; Color: clYellow;   Deep: deDeepCave;),
    // Blood Cave
    (Symbol: '^'; Name: 'Item C';              ItemType: itSword;  MaxStack: 1;    MaxDurability: 60; Color: clYellow;   Deep: deBloodCave;),
    // Dungeon of Doom
    (Symbol: '{'; Name: 'Item D';              ItemType: itSword;  MaxStack: 1;    MaxDurability: 70; Color: clYellow;   Deep: deDungeonOfDoom;)
  );
