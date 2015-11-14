unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, transymocr, Vcl.StdCtrls,tocrdll,
  Vcl.ComCtrls,system.generics.collections;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    TransymOCR1: TTransymOCR;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    procedure TransymOCR1OCRCompleted(jjob: PObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TransymOCR1OCRStatusUpdate(job: PTOCRJobInfo2; JobStatus: Integer;
      Progress: Single);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
var
  job : TOCRJob;
begin
  if(Opendialog1.Execute) then
    begin
     job                                  := TransymOCR1.NewJob;
     job.Filename                         := OpenDialog1.FileName;
     job.info.ProcessOptions.SectioningOn := True;
     job.Start;
    end;
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  job : TOCRJob;
begin
  job          := TransymOCR1.NewJob;
  job.Filename := ExtractFilePath(Application.ExeName) +'..\..\test.tif';
  job.Start;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
   TransymOCR1.OnOCRCompleted    := TransymOCR1OCRCompleted;
   TransymOCR1.OnOCRStatusUpdate := TransymOCR1OCRStatusUpdate;
   TransymOCR1.Start;
end;

procedure TForm4.FormResize(Sender: TObject);
begin
  Memo1.Left   := Button2.Left+Button2.Width + 20;
  Memo1.Width  := Width - Memo1.Left;
  Memo1.Top    := 0;
  Memo1.Height := Height;
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  Memo1.Left   := Button2.Left+Button2.Width + 20;
  Memo1.Width  := Width - Memo1.Left;
  Memo1.Top    := 0;
  Memo1.Height := Height;
end;

procedure TForm4.TransymOCR1OCRCompleted(jjob : PObject);
var
  job       : TOCRJob;
  I         : Integer;
  ocrString : String;
begin
   job := TOCRJob(jjob);
   Memo1.Lines.Add('Complete Called');
   Memo1.Lines.Add('Source Filename: '+job.Filename);
   ocrString := '';
   for I := 0 to job.OCRResultsHeader.NumItems-1 do
     begin
      if((job.OCRResultItems[i].OCRCha=13) or
         (job.OCRResultItems[i].OCRCha=10)) then
        begin
          Memo1.Lines.Add(ocrString);
          ocrString := '';
        end
      else
        begin
          ocrString := ocrString + Char(job.OCRResultItems[i].OCRCha);
        end;
     end;
end;

procedure TForm4.TransymOCR1OCRStatusUpdate(job: PTOCRJobInfo2;
  JobStatus: Integer; Progress: Single);
begin
   Memo1.Lines.Add('Status Update '+IntToStr(JobStatus)+' Completion:'+IntToStr(Trunc(Progress*100)));
end;

end.
