unit uTempSys;

interface

uses Classes;

type
  TTempSysItem = record
    Power, Duration: Word;
  end;

function TempSysItem(APower, ADuration: Word): TTempSysItem;

type
  TTempSys = class(TObject)
  private
    FList: TStringList;
    function GetText: string;
    procedure SetText(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    function VarName(I: Integer): string;
    function Power(I: Integer): Integer; overload;
    function Power(S: string): Integer; overload;
    function Duration(I: Integer): Integer; overload;
    function Duration(S: string): Integer; overload;
    function IsMove: Boolean;
    function IsVar(S: string): Boolean;
    property Text: string read GetText write SetText;
    procedure SetValue(S: string; AValue: Integer);
    procedure Add(AName: string; APower, ADuration: Integer);
    procedure Clear;
    procedure ClearVar(AName: string);
    procedure Move;
  end;

implementation

uses SysUtils;

const
  LS = '%s/%d=%d'; 

function TempSysItem(APower, ADuration: Word): TTempSysItem;
begin
  Result.Power := APower;
  Result.Duration := ADuration;
end;

{ TTempSys }

procedure TTempSys.Clear;
begin
  FList.Clear;
end;

function TTempSys.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TTempSys.Create;
begin
  FList := TStringList.Create;
  Self.Clear;
end;

destructor TTempSys.Destroy;
begin
  FList.Free;
  inherited;
end;

function TTempSys.IsMove: Boolean;
begin
  Result := (FList.Count > 0);
end;

function TTempSys.VarName(I: Integer): string;
begin
  Result := Copy(FList.Names[I], 1, Pos('/', FList.Names[I]) - 1);
end;

function TTempSys.Power(I: Integer): Integer;
begin
  Result :=StrToIntDef(Copy(FList.Names[I], Pos('/', FList.Names[I]) + 1, Length(FList.Names[I])), 0);
end;

function TTempSys.Power(S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if (S = VarName(I)) then
    begin
      Result := Power(I);
      Exit;
    end;
end;

procedure TTempSys.Move;
var
  I, V: Integer;
begin
  if IsMove then  
    with FList do   
      for I := Count - 1 downto 0 do
      begin
        V := Duration(I);
        System.Dec(V);
        if (V > 0) then
          FList[I] := Format(LS, [VarName(I), Power(I), V])
            else Delete(I);
      end;
end;

function TTempSys.Duration(I: Integer): Integer;
begin
  Result := StrToIntDef(FList.ValueFromIndex[I], 0);
end;

function TTempSys.Duration(S: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if IsMove then
    with FList do
      for I := 0 to Count - 1 do
        if (S = VarName(I)) then
        begin
          Result := StrToIntDef(Copy(FList[I], Pos('=', FList[I]) + 1, Length(FList[I])), 0);
          Exit;
        end;
end;

procedure TTempSys.SetValue(S: string; AValue: Integer);
begin
  if IsVar(S) then FList.Values[S] := IntToStr(AValue);
end;

function TTempSys.IsVar(S: string): Boolean;
begin
  Result := (Duration(S) > 0);
end;

procedure TTempSys.ClearVar(AName: string);
var
  I: Integer;
begin
  if IsMove then
    with FList do
      for I := Count - 1 to 0 do
        if (AName = VarName(I)) then
          Delete(I);
end;

procedure TTempSys.Add(AName: string; APower, ADuration: Integer);
var
  I, V, P: Integer;
begin
  if (Trim(AName) = '') or (ADuration <= 0)
    or (ADuration > 1000) or (APower <= 0)
    or (APower > 1000) then Exit;
  if IsMove then
    with FList do
      for I := 0 to Count - 1 do
      begin
        if (AName = VarName(I)) then
        begin
          P := Power(I);
          if (APower > P) then P := APower;
          V := Duration(I);
          if (ADuration > V) then V := ADuration;
          FList[I] := Format(LS, [VarName(I), P, V]);
          Exit;
        end;
      end;
  FList.Append(Format(LS, [AName, APower, ADuration]));
end;

procedure TTempSys.SetText(const AValue: string);
begin
  FList.Text := AValue;
end;

function TTempSys.GetText: string;
begin
  Result := FList.Text;
end;

end.
