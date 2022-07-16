unit Trollhunter.Scores;

interface

uses
  Classes;

type
  TScores = class(TObject)
  private
    P: array [0 .. 5] of TStringList;
    FLine: Integer;
    FMaxCount: Integer;
    procedure SetMaxCount(const Value: Integer);
    procedure SetLine(const Value: Integer);
  public
    property MaxCount: Integer read FMaxCount write SetMaxCount;
    property Line: Integer read FLine write SetLine;
    procedure Clear;
    function Count: Integer;
    function Add(Score: Integer; Name, DateTime: string;
      Level, Dungeon, Turns: Integer): Boolean;
    function GetValue(T, I: Integer): string;
    procedure Save;
    procedure Load;
    constructor Create(const AMaxCount: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  SysUtils,
  Trollhunter.Utils,
  Trollhunter.Zip,
  Trollhunter.Error,
  Trollhunter.MainForm,
  Trollhunter.Lang;

{ TScores }

function TScores.Add(Score: Integer; Name, DateTime: string;
  Level, Dungeon, Turns: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  try
    for I := 0 to P[0].Count - 1 do
    begin
      if (Score >= StrToInt(P[0][I])) then
      begin
        Line := I;
        P[0].Insert(Line, IntToStr(Score));
        P[1].Insert(Line, Name);
        P[2].Insert(Line, DateTime);
        P[3].Insert(Line, IntToStr(Level));
        P[4].Insert(Line, IntToStr(Dungeon));
        P[5].Insert(Line, IntToStr(Turns));
        Result := True;
        Save;
        Exit;
      end;
    end;
    if not Result then
    begin
      P[0].Append(IntToStr(Score));
      P[1].Append(Name);
      P[2].Append(DateTime);
      P[3].Append(IntToStr(Level));
      P[4].Append(IntToStr(Dungeon));
      P[5].Append(IntToStr(Turns));
      Line := Count - 1;
      Save;
      Exit;
    end;
  except
    on E: Exception do
      Error.Add('Scores.Add', E.Message);
  end;
end;

procedure TScores.Clear;
var
  I: Byte;
begin
  for I := 0 to High(P) do
    P[I].Clear;
end;

function TScores.Count: Integer;
begin
  Result := P[0].Count;
end;

constructor TScores.Create(const AMaxCount: Integer);
var
  I: Byte;
begin
  for I := 0 to High(P) do
    P[I] := TStringList.Create;
  MaxCount := AMaxCount;
  Line := -1;
  Clear;
end;

destructor TScores.Destroy;
var
  I: Byte;
begin
  for I := 0 to High(P) do
    P[I].Free;
  inherited;
end;

function TScores.GetValue(T, I: Integer): string;
begin
  Result := P[T][I];
  if (T = 4) then
    Result := GetMapLang(StrToInt(P[T][I]));
end;

procedure TScores.Load;
var
  I, J, C: Integer;
  A: TStringList;
  R: TExplodeResult;
  Z: TZip;
begin
  R := nil;
  if not FileExists(Path + 'save\Scores.rec') then
    Exit;
  A := TStringList.Create;
  try
    try
      Clear;
      Z := TZip.Create(MainForm);
      try
        A.Text := Z.ExtractTextFromFile(Path + 'save\Scores.rec', 'scores.txt');
      finally
        Z.Free;
      end;
      C := A.Count - 1;
      if C > MaxCount - 1 then
        C := MaxCount - 1;
      for I := 0 to C do
      begin
        R := Explode(',', A[I]);
        for J := 0 to High(P) do
          P[J].Append(Trim(R[J]));
      end;
    except
      on E: Exception do
        Error.Add('Scores.Load', E.Message);
    end;
  finally
    FreeAndNil(A);
  end;
end;

procedure TScores.Save;
var
  I, J, C: Integer;
  A: TStringList;
  S, K: string;
  Z: TZip;
begin
  A := TStringList.Create;
  try
    C := EnsureRange(Count - 1, 0, MaxCount - 1);
    for I := 0 to C do
      try
        S := '';
        for J := 0 to High(P) do
        begin
          if (J < High(P)) then
            K := ','
          else
            K := '';
          S := S + P[J][I] + K;
        end;
        A.Append(S);
        Z := TZip.Create(MainForm);
        try
          Z.Password := PWD;
          Z.FileName := Path + 'save\Scores.rec';
          Z.OpenArchive;
          Z.AddFromString('scores.txt', A.Text);
          Z.CloseArchive;
        finally
          Z.Free;
        end;
      except
        on E: Exception do
          Error.Add('Scores.Save', E.Message);
      end;
  finally
    FreeAndNil(A);
  end;
end;

procedure TScores.SetLine(const Value: Integer);
begin
  FLine := Value;
end;

procedure TScores.SetMaxCount(const Value: Integer);
begin
  FMaxCount := Value;
end;

end.
