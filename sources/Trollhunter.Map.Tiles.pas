unit Trollhunter.Map.Tiles;

interface

uses
  Graphics,
  Trollhunter.Color;

type
  Tiles = (tlMin,
           tlFloor, 
           tlGrass,
           tlStone,
           tlEmpty,
           tlWall,
           tlOpenDoor,
           tlClosedDoor, 
           tlClosedDoorWithFireTrap, 
           tlClosedDoorWithLightningTrap,
           tlLockedDoor, 
           tlHiddenDoor, 
           tlPrevDungeon, 
           tlNextDungeon,
           tlAltNextDungeon,
           tlEmptyShrine, 
           tlLifeShrine, 
           tlManaShrine, 
           tlMegaShrine,
           tlOpenWoodChest, 
           tlClosedWoodChest, 
           tlLockedWoodChest, 
           tlOpenBestChest,
           tlLockedBestChest, 
           tlOpenBarrel, 
           tlClosedBarrel,
           tlTree,
           tlBush,
           tlMax);

type
  TTile = record
    Tile : Char;
    Color: Integer;
  end;
          
const
  Tile: array [tlMin..tlMax] of TTile = (
  (Tile:  #0; Color: cBlack),
  (Tile: '.'; Color: cDkGray), 
  (Tile: ','; Color: cDkGreen),
  (Tile: ';'; Color: cGray),
  (Tile: ' '; Color: cBlack),
  (Tile: '#'; Color: cDkBrown),
  (Tile: '-'; Color: cDkGray),
  (Tile: '+'; Color: cLtBrown),
  (Tile: '/'; Color: cLtBrown),
  (Tile: '\'; Color: cLtBrown),
  (Tile: '|'; Color: cLtBrown),
  (Tile: '_'; Color: cDkBrown),
  (Tile: '<'; Color: cDkRed),
  (Tile: '>'; Color: clRed),
  (Tile: '*'; Color: clRed),
  (Tile: 'Q'; Color: cLtGray),
  (Tile: 'W'; Color: cLtRed),
  (Tile: 'E'; Color: cLtBlue),
  (Tile: 'R'; Color: cRdYellow),
  (Tile: 'U'; Color: cLtGray),
  (Tile: 'Y'; Color: cLtPurple),
  (Tile: 'I'; Color: cLtPurple),
  (Tile: 'O'; Color: cLtGray),
  (Tile: 'P'; Color: cLtPurple),
  (Tile: '['; Color: cLtGray),
  (Tile: ']'; Color: cLtPurple),
  (Tile: 'T'; Color: cRdGreen),
  (Tile: 't'; Color: cLtGreen),
  (Tile: #255; Color: cBlack));

implementation

end.
