unit Trollhunter.Script;

interface

uses
  SysUtils,
  Trollhunter.Utils,
  Types,
  Classes;

type
  TScript = class(TObject)
  private
    FL: TStringList;
    FFileName: string;
    procedure ShowError(Operator: string; Line: Integer);
  public
    procedure Run;
    procedure Clear;
    procedure LoadFromFile(FileName: string);
    constructor Create; overload;
    constructor Create(S: string); overload;
    destructor Destroy; override;
  end;

implementation

uses
  Dialogs,
  Trollhunter.Map,
  Trollhunter.Creatures,
  Trollhunter.Error,
  Trollhunter.Item;

{ TScript }

constructor TScript.Create;
begin
  FL := TStringList.Create;
  FFileName := '';
end;

procedure TScript.Clear;
begin
  FFileName := '';
end;

constructor TScript.Create(S: string);
begin
  Create;
  FL.Text := S;
end;

destructor TScript.Destroy;
begin
  FL.Free;
  inherited;
end;

procedure TScript.LoadFromFile(FileName: string);
begin
  FFileName := FileName;
  FL.LoadFromFile(FFileName);
end;

procedure TScript.Run;
var
  I, J: Integer;
  R: TExplodeResult;
begin
  R := nil;
  if (FL.Count > 0) then
    for I := 0 to FL.Count - 1 do
    begin
      FL[I] := Trim(FL[I]);
      if (FL[I] = '') or (FL[I][1] = ';') or (Pos(' ', FL[I]) < 1) then
        Continue;
      R := Explode(' ', FL[I]);
      for J := 0 to High(R) do
        R[J] := Trim(R[J]);
      // MapLevel
      try
        if (LowerCase(R[0]) = 'maplevel') then
          Map.Level := StrToInt(R[1]);
      except
        ShowError('MapLevel', I);
      end;
      // MapItems
      try
        if (LowerCase(R[0]) = 'mapitems') then
          Map.Items := R[1];
      except
        ShowError('MapItems', I);
      end;
      // MapCreatures
      try
        if (LowerCase(R[0]) = 'mapcreatures') then
          Map.Creatures := R[1];
      except
        ShowError('MapCreatures', I);
      end;
      // AddCreature
      try
        if (LowerCase(R[0]) = 'addcreature') then
          with Creatures do
          begin
            Add(StrToInt(R[1]), StrToInt(R[2]), R[3]);
            if (Length(R) >= 5) then
              Enemy[High(Enemy)].Life.SetCur(StrToInt(R[4]));
            if (Length(R) >= 6) then
              Enemy[High(Enemy)].Mana.SetCur(StrToInt(R[5]));
          end;
      except
        ShowError('AddCreature', I);
      end;
      // AddItem
      try
        if (LowerCase(R[0]) = 'additem') then
          with Items do
          begin
            Add(StrToInt(R[1]), StrToInt(R[2]), R[3]);
            if (Length(R) >= 5) then
              Item[High(Item)].Prop.Tough := StrToInt(R[4]);
            if (Length(R) >= 6) then
              Item[High(Item)].Count := StrToInt(R[5]);
          end;
      except
        ShowError('AddItem', I);
      end;
    end;
end;

procedure TScript.ShowError(Operator: string; Line: Integer);
begin
  ShowMessage(Format('Operator <%s> except error!' + #13 + 'Script file: "%s"' +
    #13 + 'Line: %d' + #13 + 'Code: %s', [Operator, FFileName, Line,
    FL[Line]]));
  Halt;
end;

end.
