(*
   EnumHelper by ortuagustin https://codegists.com/user/ortuagustin
   English translation for comments - by Karagy
*)

unit EnumHelper;

interface

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti;

type
{$REGION 'Exception types'}
  EEnumOutOfRange = class(System.SysUtils.EArgumentOutOfRangeException);
  EEnumNameNotFound = class(System.SysUtils.Exception);
  EAttributeNotFound = class(System.SysUtils.Exception);
{$ENDREGION}

{$REGION 'EnumNamesAttribute'}
  /// <summary> This attribute allows you to annotate an enumerative type with a list of values that identify
  /// to each value of that type </summary>
  /// <summary> The constructor receives an identifier and a string delimited by a character, it could be seen
  /// to that string as an "array". This is implemented due to a limitation of Delphi, since it is not allowed
  /// initialize attributes with fixes (even if they are constant) </summary>
  /// <summary> To get the value you should use the Enum helper <T> .EnumName (Identifier, Value) </summary>
  /// <summary> An example of serious use: </summary>
  /// <summary> type </summary>
  /// <summary> [EnumNames ('TestEnumAttribute', 'Hello, World')] </summary>
  /// <summary> TTestEnumeration = (First, Second); </summary>
  /// <summary> Enum <TTestEnumeration> .EnumName ('TestEnumAttribute', TTestEnumeration.First) -> 'Hello' </summary>
  EnumNamesAttribute = class(TCustomAttribute)
  strict private
    FIdentifier: string;
    FNames: TArray<string>;
  public
    constructor Create(const Identifier, Names: string; const Delimiter: string = ',');
    function NameOf<T: record {: enum}>(const Value: T): string;
    property Identifier: string read FIdentifier;
    property Names: TArray<string> read FNames;
  end;
{$ENDREGION}

{$REGION 'Enum<T>'}
  /// <summary> Record that contains static methods to work with enums types </summary>
  Enum<T: record {: enum}> = record
  strict private
    class function EnumTypeInfo: PTypeInfo; static; inline;
    class function EnumTypeData: PTypeData; static; inline;

    class function EnumNamesAttributes: TArray<EnumNamesAttribute>; static;
    class function EnumNameAttribute(const Identifier: string): EnumNamesAttribute; static;
    class function TryGetEnumNameAttribute(const Identifier: string; out Attribute: EnumNamesAttribute): Boolean; static;
    class function TryGetEnumName(const Identifier: string; const Value: T; out Name: string): Boolean; static;

    class procedure AttributeNotFound(const Identifier: string); static;
    class procedure NameNotFound(const Identifier: string; const Value: T); static;
    class procedure OutOfRange(const Value: T; const Namespace, MethodName: string); static;
  public
    /// <summary> The name of the enum type </summary>
    class function TypeName: string; static; inline;
    /// <summary> The name of the enum value </summary>
    class function ValueName(const Value: T): string; static; inline;
    /// <summary> Returns the value of the enum type annotated by the EnumNames attribute </summary>
    /// <summary> If the enum is not annotated by the EnumNames attribute, or is not annotated by an attribute
    /// EnumNames with the indicated identifier, an exception is raised EEnumNameNotFound </summary>
    /// <remarks> See EnumNamesAttribute </remarks>
    class function EnumName(const Identifier: string; const Value: T): string; static; inline;
    /// <summary> Returns the value of the enum type annotated by the EnumNames attribute </summary>
    /// <summary> Instead of raising an EEnumNameNotFound exception, the Default value is returned </summary>
    /// <summary> If Default = EmptyStr, ValueName (Value) is returned </summary>
    /// <remarks> See EnumNamesAttribute </remarks>
    class function EnumNameOrDefault(const Identifier: string; const Value: T; const Default: string = ''): string; static; inline;
    /// <summary> Returns all the names with which the enum was written down </summary>
    class function EnumNames(const Identifier: string): TArray<string>; static; inline;
    /// <summary> Returns the enum value given an Ordinal </summary>
    class function Parse(const Ordinal: Integer): T; static; inline;
    /// <summary> Convert the value enum to its corresponding Ordinal </summary>
    class function ToInteger(const Value: T): Integer; static; inline;
    /// <summary> The maximum value of the enum. Equivalent to Ord (High (T)) </summary>
    class function MaxValue: Integer; static; inline;
    /// <summary> The minimum value of the enum. Equivalent to Ord (Low (T)) </summary>
    class function MinValue: Integer; static; inline;
    /// <summary> Returns True if the value of the enum type is within the allowed range </summary>
    class function InRange(const Value: T): Boolean; overload; static;
    /// <summary> Returns True if the integer is within the allowed range of the enum type </summary>
    class function InRange(const Value: Integer): Boolean; overload; static;
    /// <summary> Raise an exception EEnumOutOfRange if the value of the enum type is out of range allowed </summary>
    /// <param name = "Value"> The value to test </param>
    /// <param name = "Namespace"> Describes the "context" of the person invoking this method (eg class or unit) </param>
    /// <param name = "MethodName"> Name of the method that invoked this routine </param>
    class procedure CheckInRange(const Value: T; const Namespace, MethodName: string); static;
    /// <summary> Amount of elements of the enum </summary>
    class function Count: Integer; static;
    /// <summary> Returns an Array with the elements of the enum </summary>
    class function AsArray: TArray<T>; static;
  end;
{$ENDREGION}

{$REGION 'Rtti'}
  /// <summary> It maintains a private TRttiContext instance to which it delegates public methods </summary>
  Rtti = record
  public type
    TAttributeClass = class of TCustomAttribute;
    TRttiMemberPredicate = reference to function(const RttiMember: TRttiMember): Boolean;
  strict private
    class var FContext: TRttiContext;
    class var ContextSentinel: string;
    class function GetContext: TRttiContext; static;
    class property Context: TRttiContext read GetContext;
  public
    /// <summary> Gets the TRttiType of the class of a given object </summary>
    class function GetObjectRtti(const AObject: TObject): TRttiType; static;
    /// <summary> Get the TRttiType for a given class </summary>
    class function GetClassRtti(const AClass: TClass): TRttiType; static;
    /// <summary> Get the TRttiType for a certain type </summary>
    class function GetTypeRtti(const ATypeInfo: PPTypeInfo): TRttiType; static;
    /// <summary> Get the TRttiType for an enum </summary>
    class function GetEnumRtti<T: record {: enum}>: TRttiType; static;
  end;
{$ENDREGION}

{$REGION 'TRttiNamedObjectHelper'}
  TRttiNamedObjectHelper = class helper for System.Rtti.TRttiNamedObject
  strict private
    procedure AttributeNotFoundError(const AttributeName: string);
  public
    /// <summary> Returns the attribute of the class determined by the generic type </summary>
    /// <remarks> Raise an EAttributeNotFound exception if the type is not annotated with that attribute </remarks>
    function GetAttribute<T: TCustomAttribute>: T;
    /// <summary> This method is already provided by the RTTI of Delphi, but I need to overload it because the
    /// generic version hides it. </summary>
    function GetAttributes: TArray<TCustomAttribute>; overload;
    /// <summary> Returns all attributes of the class determined by the generic type </summary>
    function GetAttributes<T: TCustomAttribute>: TArray<T>; overload;
  end;
{$ENDREGION}

implementation

uses
  System.Types,
  System.Math,
  System.StrUtils,
  System.Generics.Collections;

{$REGION 'Enum<T>'}

class function Enum<T>.InRange(const Value: T): Boolean;
begin
  Result := InRange(ToInteger(Value));
end;

class function Enum<T>.InRange(const Value: Integer): Boolean;
begin
  Result := System.Math.InRange(Value, Enum<T>.MinValue, Enum<T>.MaxValue);
end;

class function Enum<T>.MaxValue: Integer;
begin
  Result := Enum<T>.EnumTypeData.MaxValue;
end;

class function Enum<T>.MinValue: Integer;
begin
  Result := Enum<T>.EnumTypeData.MinValue;
end;

class function Enum<T>.ToInteger(const Value: T): Integer;
begin
  Result := 0;
  System.Move(Value, Result, System.SizeOf(Value));
end;

class function Enum<T>.TypeName: string;
begin
  Result := string(Enum<T>.EnumTypeInfo.Name);
end;

class function Enum<T>.ValueName(const Value: T): string;
begin
  Result := System.TypInfo.GetEnumName(Enum<T>.EnumTypeInfo, Enum<T>.ToInteger(Value));
end;

class function Enum<T>.EnumTypeData: PTypeData;
begin
  Result := System.TypInfo.GetTypeData(Enum<T>.EnumTypeInfo);
end;

class function Enum<T>.EnumTypeInfo: PTypeInfo;
begin
  Result := System.TypeInfo(T);
end;

class procedure Enum<T>.CheckInRange(const Value: T; const Namespace, MethodName: string);
begin
  if not Enum<T>.InRange(Value) then
    Enum<T>.OutOfRange(Value, Namespace, MethodName);
end;

class function Enum<T>.Count: Integer;
begin
  Result := Enum<T>.MaxValue - Enum<T>.MinValue + 1;
end;

class procedure Enum<T>.OutOfRange(const Value: T; const Namespace, MethodName: string);
const
  SEnumOutOfRange = '%s.%s :: %d is out of range for enum %s';
begin
  raise EEnumOutOfRange.CreateFmt(SEnumOutOfRange, [Namespace, MethodName, ToInteger(Value), TypeName]);
end;

class function Enum<T>.Parse(const Ordinal: Integer): T;
begin
  Assert(System.SizeOf(Result) <= System.SizeOf(Ordinal));
  Move(Ordinal, Result, System.SizeOf(Result));
end;

class function Enum<T>.EnumNamesAttributes: TArray<EnumNamesAttribute>;
begin
  Result := Rtti.GetEnumRtti<T>.GetAttributes<EnumNamesAttribute>;
end;

class function Enum<T>.TryGetEnumNameAttribute(const Identifier: string; out Attribute: EnumNamesAttribute): Boolean;
var
  Attributes: TArray<EnumNamesAttribute>;
  Each: EnumNamesAttribute;
begin
  Attributes := EnumNamesAttributes;

  for Each in Attributes do
  begin
    if Each.Identifier = Identifier then
    begin
      Attribute := Each;
      Exit(True);
    end;
  end;

  Result := False;
end;

class function Enum<T>.EnumNameAttribute(const Identifier: string): EnumNamesAttribute;
begin
  if not Enum<T>.TryGetEnumNameAttribute(Identifier, Result) then
    AttributeNotFound(Identifier);
end;

class function Enum<T>.TryGetEnumName(const Identifier: string; const Value: T; out Name: string): Boolean;
var
  Attribute: EnumNamesAttribute;
begin
  if Enum<T>.TryGetEnumNameAttribute(Identifier, Attribute) then
  begin
    Name := Attribute.NameOf<T>(Value);
    Result := True;
  end
  else
    Result := False;
end;

class function Enum<T>.EnumNames(const Identifier: string): TArray<string>;
var
  I: Integer;
  Attribute: EnumNamesAttribute;
begin
  Attribute := EnumNameAttribute(Identifier);
  System.SetLength(Result, System.Length(Attribute.Names));
  for I := System.Low(Attribute.Names) to System.High(Attribute.Names) do
    Result[I] := Attribute.Names[I];
end;

class function Enum<T>.EnumName(const Identifier: string; const Value: T): string;
begin
  if not Enum<T>.TryGetEnumName(Identifier, Value, Result) then
    NameNotFound(Identifier, Value);
end;

class function Enum<T>.EnumNameOrDefault(const Identifier: string; const Value: T; const Default: string): string;
begin
  if not Enum<T>.TryGetEnumName(Identifier, Value, Result) then
  begin
    if Default.IsEmpty then
      Result := Enum<T>.ValueName(Value)
    else
      Result := Default;
  end;
end;

class procedure Enum<T>.NameNotFound(const Identifier: string; const Value: T);
const
  SEnumNameNotFound = 'EnumName not found for %s.%s with identifier %s';
begin
  raise EEnumNameNotFound.CreateFmt(SEnumNameNotFound, [TypeName, ValueName(Value), Identifier]);
end;

class procedure Enum<T>.AttributeNotFound(const Identifier: string);
const
  SAttributeNotFound = '%s is not annotated with EnumName Attribuye with identifier %s';
begin
  raise EEnumNameNotFound.CreateFmt(SAttributeNotFound, [TypeName, Identifier]);
end;

class function Enum<T>.AsArray: TArray<T>;
var
  I: Integer;
begin
  System.SetLength(Result, Enum<T>.Count);
  for I := System.Low(Result) to System.High(Result) do
    Result[I] := Enum<T>.Parse(I);
end;

{$ENDREGION}

{$REGION 'EnumNamesAttribute'}

constructor EnumNamesAttribute.Create(const Identifier, Names: string; const Delimiter: string = ',');
var
  Index: Integer;
  SplitValues: System.Types.TStringDynArray;
begin
  inherited Create;
  FIdentifier := Identifier;
  SplitValues := System.StrUtils.SplitString(Names, Delimiter);
  System.SetLength(FNames, System.Length(SplitValues));
  for Index := System.Low(SplitValues) to System.High(SplitValues) do
    FNames[Index] := SplitValues[Index];
end;

function EnumNamesAttribute.NameOf<T>(const Value: T): string;
var
  Index: Integer;
begin
  Index := Enum<T>.ToInteger(Value);
  if System.Math.InRange(Index, System.Low(FNames), System.High(FNames)) then
    Result := FNames[Index]
  else
    Result := System.SysUtils.EmptyStr;
end;

{$ENDREGION}

{$REGION 'Rtti'}

class function Rtti.GetContext: TRttiContext;
begin
  if ContextSentinel = EmptyStr then
  begin
    FContext := TRttiContext.Create;
    ContextSentinel := '@';
  end;

  Result := FContext;
end;

class function Rtti.GetEnumRtti<T>: TRttiType;
begin
  Result := GetTypeRtti(TypeInfo(T));
end;

class function Rtti.GetTypeRtti(const ATypeInfo: PPTypeInfo): TRttiType;
begin
  Result := Context.GetType(ATypeInfo);
end;

class function Rtti.GetClassRtti(const AClass: TClass): TRttiType;
begin
  Result := Context.GetType(AClass.ClassInfo);
end;

class function Rtti.GetObjectRtti(const AObject: TObject): TRttiType;
begin
  Result := GetClassRtti(AObject.ClassType);
end;

{$ENDREGION}

{$REGION 'TRttiNamedObjectHelper'}

procedure TRttiNamedObjectHelper.AttributeNotFoundError(const AttributeName: string);
begin
  raise EAttributeNotFound.Create('Attribute: ' + AttributeName + ' not found');
end;

function TRttiNamedObjectHelper.GetAttribute<T>: T;
var
  Each: TCustomAttribute;
begin
  for Each in GetAttributes do
  begin
    if Each is T then
      Exit(Each as T);
  end;

  AttributeNotFoundError(T.QualifiedClassName);
end;

function TRttiNamedObjectHelper.GetAttributes: TArray<TCustomAttribute>;
begin
  Result := inherited GetAttributes;
end;

function TRttiNamedObjectHelper.GetAttributes<T>: TArray<T>;
var
  Items: TList<T>;
  Each: TCustomAttribute;
begin
  Items := TList<T>.Create;
  try
    for Each in GetAttributes do
    begin
      if Each is T then
        Items.Add(Each);
    end;

    Result := Items.ToArray;
  finally
    Items.Free;
  end;
end;

{$ENDREGION}

end.
