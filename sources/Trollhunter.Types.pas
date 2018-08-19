unit Trollhunter.Types;

interface

type
  Int = System.NativeInt;
  UInt = System.Word;

const
  UIntMax = High(Byte);

type
  TMinMax = record
    Min: UInt;
    Max: UInt;
  end;

implementation

end.
