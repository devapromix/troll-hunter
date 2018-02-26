unit uUtils;

interface

uses
  Forms;

type
  Utils = class(TObject)
  public
    class function GetPath(SubDir: string): string;
    class function ShowCenterForm(const Form: TForm; Show: Boolean = True): Integer;
  end;

implementation

uses SysUtils;

class function Utils.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

class function Utils.ShowCenterForm(const Form: TForm; Show: Boolean = True): Integer;
begin
  Result := -1;
  Form.BorderStyle := bsDialog;
  Form.Left := Application.MainForm.Left + ((Application.MainForm.Width div 2) - (Form.Width div 2));
  Form.Top := Application.MainForm.Top + ((Application.MainForm.Height div 2) - (Form.Height div 2));
  if Show then
    Result := Form.ShowModal;
end;

end.
