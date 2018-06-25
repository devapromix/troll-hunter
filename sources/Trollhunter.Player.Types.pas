unit Trollhunter.Player.Types;

interface

uses System.Types;

type
  TDirectionEnum = (drEast, drWest, drSouth, drNorth, drSouthEast, drSouthWest,
    drNorthEast, drNorthWest, drOrigin);

const
  Direction: array [TDirectionEnum] of TPoint = ((X: 1; Y: 0), (X: - 1; Y: 0),
    (X: 0; Y: 1), (X: 0; Y: - 1), (X: 1; Y: 1), (X: - 1; Y: 1), (X: 1; Y: - 1),
    (X: - 1; Y: - 1), (X: 0; Y: 0));

type
  TSexEnum = (sxMale, sxFemale);

type
  TSlotType = (stNone, stHead, stTorso, stHands, stFeet, stMainHand, stOffHand,
    stNeck, stFinger, stTorch);

implementation

end.
