{****************************************************}
{                                                    }
{  Generics.Tuples                                   }
{                                                    }
{  Copyright (C) 2014 Malcolm Groves                 }
{                                                    }
{  https://github.com/malcolmgroves/generics.tuples  }
{                                                    }
{****************************************************}
{                                                    }
{  This Source Code Form is subject to the terms of  }
{  the Mozilla Public License, v. 2.0. If a copy of  }
{  the MPL was not distributed with this file, You   }
{  can obtain one at                                 }
{                                                    }
{  http://mozilla.org/MPL/2.0/                       }
{                                                    }
{****************************************************}
unit Generics.Tuples;

interface

type
  ITuple<T1, T2> = interface
    procedure SetValue1(Value : T1);
    function GetValue1 : T1;
    procedure SetValue2(Value : T2);
    function GetValue2 : T2;
    property Value1 : T1 read GetValue1 write SetValue1;
    property Value2 : T2 read GetValue2 write SetValue2;
  end;

  ITuple<T1, T2, T3> = interface(ITuple<T1, T2>)
    procedure SetValue3(Value : T3);
    function GetValue3 : T3;
    property Value3 : T3 read GetValue3 write SetValue3;
  end;

  TTuple<T1, T2> = class(TInterfacedObject, ITuple<T1, T2>)
  protected
    FValue1 : T1;
    FValue2 : T2;
    procedure SetValue1(Value : T1);
    function GetValue1 : T1;
    procedure SetValue2(Value : T2);
    function GetValue2 : T2;
  public
    constructor Create(Value1 : T1; Value2 : T2); virtual;
    destructor Destroy; override;
    property Value1 : T1 read FValue1 write FValue1;
    property Value2 : T2 read FValue2 write FValue2;
  end;

  TTuple<T1, T2, T3> = class(TTuple<T1, T2>, ITuple<T1, T2, T3>)
  protected
    FValue1 : T1;
    FValue2 : T2;
    FValue3 : T3;
    procedure SetValue3(Value : T3);
    function GetValue3 : T3;
  public
    constructor Create(Value1 : T1; Value2 : T2; Value3 : T3); reintroduce;
    destructor Destroy; override;
    property Value3 : T3 read GetValue3 write SetValue3;
  end;

implementation
uses
  System.RTTI;

{ TPair<T1, T2> }

constructor TTuple<T1, T2>.Create(Value1: T1; Value2: T2);
begin
  self.Value1 := Value1;
  self.Value2 := Value2;
end;

destructor TTuple<T1, T2>.Destroy;
{$IFNDEF AUTOREFCOUNT}
var
  LValue1Holder, LValue2Holder : TValue;
{$ENDIF}
begin
{$IFNDEF AUTOREFCOUNT}
  LValue1Holder := TValue.From<T1>(FValue1);
  if LValue1Holder.IsObject then
    LValue1Holder.AsObject.Free;

  LValue2Holder := TValue.From<T2>(FValue2);
  if LValue2Holder.IsObject then
    LValue2Holder.AsObject.Free;
  inherited;
{$ENDIF}
end;

function TTuple<T1, T2>.GetValue1: T1;
begin
  Result := FValue1;
end;

function TTuple<T1, T2>.GetValue2: T2;
begin
  Result := FValue2;
end;

procedure TTuple<T1, T2>.SetValue1(Value: T1);
begin
  FValue1 := Value;
end;

procedure TTuple<T1, T2>.SetValue2(Value: T2);
begin
  FValue2 := Value;
end;

{ TTuple3<T1, T2, T3> }

constructor TTuple<T1, T2, T3>.Create(Value1: T1; Value2: T2; Value3: T3);
begin
  inherited Create(Value1, Value2);
  self.Value3 := Value3;
end;

destructor TTuple<T1, T2, T3>.Destroy;
{$IFNDEF AUTOREFCOUNT}
var
  LValue3Holder : TValue;
{$ENDIF}
begin
{$IFNDEF AUTOREFCOUNT}
  LValue3Holder := TValue.From<T3>(FValue3);
  if LValue3Holder.IsObject then
    LValue3Holder.AsObject.Free;
{$ENDIF}
  inherited;
end;

function TTuple<T1, T2, T3>.GetValue3: T3;
begin
  Result := FValue3;
end;

procedure TTuple<T1, T2, T3>.SetValue3(Value: T3);
begin
  FValue3 := Value;
end;

end.
