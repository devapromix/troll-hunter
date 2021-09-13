unit uBox;

interface

uses Windows, SysUtils;

procedure Box(); overload;
procedure Box(const BoxStrMessage: string); overload;
procedure Box(const BoxIntMessage: Integer); overload;
procedure Box(const BoxBoolMessage: Boolean); overload;
procedure Box(const BoxStrMessage, Title: string); overload;

implementation

procedure Box(); overload;
begin
  MessageBox(0, '', 'Info', MB_OK);
end;

procedure Box(const BoxStrMessage: string); overload;
begin
  MessageBox(0, PChar(BoxStrMessage), 'Info', MB_OK);
end;

procedure Box(const BoxIntMessage: Integer); overload;
begin
  MessageBox(0, PChar(IntToStr(BoxIntMessage)), 'Info', MB_OK);
end;

procedure Box(const BoxBoolMessage: Boolean); overload;
begin
  MessageBox(0, PChar(BoolToStr(BoxBoolMessage)), 'Info', MB_OK);
end;

procedure Box(const BoxStrMessage, Title: string);
begin
  MessageBox(0, PChar(BoxStrMessage), PChar(Title), MB_OK);
end;

end.
