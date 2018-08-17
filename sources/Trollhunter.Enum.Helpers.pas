unit Trollhunter.Enum.Helpers;
 
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
  /// <summary> Este atributo permite anotar un tipo enumerativo con una lista de valores que identifican
  /// a cada valor de dicho tipo </summary>
  /// <summary> El constructor recibe un identificador y un string delimitado por un caracter, podria verse
  /// a ese string como un "array". Esto es implementado asi debido a una limitacion de Delphi, ya que no se permite
  /// inicializar atributos con arreglos (aunque sean constantes) </summary>
  /// <summary> Para obtener el valor se debe usar el ayudante Enum<T>.EnumName(Identificador, Valor) </summary>
  /// <summary> Un ejemplo de uso seria: </summary>
  /// <summary> type </summary>
  /// <summary>   [EnumNames('TestEnumAttribute', 'Hello, World')] </summary>
  /// <summary>   TTestEnumeration = (First, Second); </summary>
  /// <summary> Enum<TTestEnumeration>.EnumName('TestEnumAttribute', TTestEnumeration.First) --> 'Hello' </summary>
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
  /// <summary> Record que contiene metodos estaticos para trabajar con tipos enums </summary>
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
    /// <summary> El nombre del tipo enum </summary>
    class function TypeName: string; static; inline;
    /// <summary> El nombre del valor enum </summary>
    class function ValueName(const Value: T): string; static; inline;
    /// <summary> Devuelve el valor del tipo enum anotado por el atributo EnumNames </summary>
    /// <summary> Si el enum no esta anotado por el atributo EnumNames, o no esta anotado por un atributo
    /// EnumNames con el identificador indicado, se eleva una excepcion EEnumNameNotFound </summary>
    /// <remarks> Ver EnumNamesAttribute </remarks>
    class function EnumName(const Identifier: string; const Value: T): string; static; inline;
    /// <summary> Devuelve el valor del tipo enum anotado por el atributo EnumNames </summary>
    /// <summary> En lugar de elevar una excepcion EEnumNameNotFound, se devuelve el valor Default </summary>
    /// <summary> Si Default = EmptyStr se devuelve ValueName(Value) </summary>
    /// <remarks> Ver EnumNamesAttribute </remarks>
    class function EnumNameOrDefault(const Identifier: string; const Value: T; const Default: string = ''): string; static; inline;
    /// <summary> Devuelve todos los nombres con los que fue anotado el enum </summary>
    class function EnumNames(const Identifier: string): TArray<string>; static; inline;
    /// <summary> Devuelve el valor enum dado un Ordinal </summary>
    class function Parse(const Ordinal: Integer): T; static; inline;
    /// <summary> Convierte el valor enum a su correspondiente Ordinal </summary>
    class function ToInteger(const Value: T): Integer; static; inline;
    /// <summary> El valor maximo del enum. Equivalente a Ord(High(T)) </summary>
    class function MaxValue: Integer; static; inline;
    /// <summary> El valor maximo del enum. Equivalente a Ord(Low(T)) </summary>
    class function MinValue: Integer; static; inline;
    /// <summary> Devuelve True si el valor del tipo enum se encuentra dentro del rango permitido </summary>
    class function InRange(const Value: T): Boolean; overload; static;
    /// <summary> Devuelve True si el entero se encuentra dentro del rango permitido del tipo enum </summary>
    class function InRange(const Value: Integer): Boolean; overload; static;
    /// <summary> Eleva una excepcion EEnumOutOfRange si el valor del tipo enum esta fuera del rango
    // permitido </summary>
    /// <param name="Value"> El valor a testear </param>
    /// <param name="Namespace"> Describe el "contexto" de quien invoca a este metodo (ej clase o unidad) </param>
    /// <param name="MethodName"> Nombre del metodo que invoco a esta rutina </param>
    class procedure CheckInRange(const Value: T; const Namespace, MethodName: string); static;
    /// <summary> Cantidad de elementos del enum </summary>
    class function Count: Integer; static;
    /// <summary> Devuelve un Array con los elementos del enum </summary>
    class function AsArray: TArray<T>; static;
  end;
{$ENDREGION}
 
{$REGION 'Rtti'}
  /// <summary> Mantiene una instancia de TRttiContext privada al que delega los metodos publicos </summary>
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
    /// <summary> Obtiene el TRttiType de la clase de una objeto determinado </summary>
    class function GetObjectRtti(const AObject: TObject): TRttiType; static;
    /// <summary> Obtiene el TRttiType para un clase determinada </summary>
    class function GetClassRtti(const AClass: TClass): TRttiType; static;
    /// <summary> Obtiene el TRttiType para un tipo determinado </summary>
    class function GetTypeRtti(const ATypeInfo: PPTypeInfo): TRttiType; static;
    /// <summary> Obtiene el TRttiType para un enum </summary>
    class function GetEnumRtti<T: record {: enum}>: TRttiType; static;
  end;
{$ENDREGION}
 
{$REGION 'TRttiNamedObjectHelper'}
  TRttiNamedObjectHelper = class helper for System.Rtti.TRttiNamedObject
  strict private
    procedure AttributeNotFoundError(const AttributeName: string);
  public
    /// <summary> Devuelve el atributo de la clase determinada por el tipo generico </summary>
    /// <remarks> Eleva una excepcion EAttributeNotFound si el tipo no esta anotado con ese atributo </remarks>
    function GetAttribute<T: TCustomAttribute>: T;
    /// <summary> Este metodo ya lo provee la RTTI de Delphi, pero necesito sobrecargarlo porque la version
    /// generica lo oculta </summary>
    function GetAttributes: TArray<TCustomAttribute>; overload;
    /// <summary> Devuelve todos los atributos de la clase determinada por el tipo generico </summary>
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