unit Trollhunter.Scene;

interface

uses Classes;

type
  TScene = class(TObject)
    procedure Render(); virtual; abstract;
    procedure KeyDown(var Key: Word; Shift: TShiftState); virtual; abstract;
    procedure KeyPress(var Key: Char); virtual; abstract;
  end;

implementation

end.
