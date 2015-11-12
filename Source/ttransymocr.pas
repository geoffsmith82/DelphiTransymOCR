unit ttransymocr;

interface

uses
  System.SysUtils, System.Classes;

type
  TTransymOCR = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tyson Technology', [TTransymOCR]);
end;

end.
