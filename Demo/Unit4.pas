unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, transymocr, Vcl.StdCtrls,tocrdll,
  Vcl.ComCtrls,system.generics.collections, Vcl.FileCtrl;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    TransymOCR1: TTransymOCR;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button3: TButton;
    ListView1: TListView;
    DirectoryListBox1: TDirectoryListBox;
    procedure TransymOCR1OCRCompleted(var job:IOCRJob);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TransymOCR1OCRStatusUpdate(var job:IOCRJob; JobStatus: Integer;
      Progress: Single);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  job  : TOCRJob;
  item : TListItem;
begin
  if(Opendialog1.Execute) then
    begin
     job                                   := TOCRJob(TransymOCR1.NewJob);
     job.Filename                          := OpenDialog1.FileName;
     item := Listview1.Items.Add;
     item.Caption := ExtractFilename(job.Filename) +' ' + 'Queued';
     job.Finfo.ProcessOptions.SectioningOn := True;
     job.ExtraObject := item;
     job.Start;
    end;
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  job  : IOCRJob;
  item : TListItem;
begin
  job             := TOCRJob(TransymOCR1.NewJob);
  job.Filename    := 'C:\Program Files (x86)\Transym\TOCR\Viewer\Samples\Sample.tif';
  item            := Listview1.Items.Add;
  item.Caption    := ExtractFilename(job.Filename) +' ' + 'Queued';
  job.ExtraObject := item;
  job.Start;
end;

procedure TForm4.Button3Click(Sender: TObject);
var
  job          : TOCRJob;
  item         : TListItem;
  item2        : TListItem;
  searchResult : TSearchRec;
begin
  findfirst(IncludeTrailingPathDelimiter(DirectoryListBox1.Directory)+'*',faAnyFile,searchResult);
 try
  repeat
    if((UpperCase(ExtractFileExt(searchResult.Name))='.BMP') or
       (UpperCase(ExtractFileExt(searchResult.Name))='.TIF')) then
      begin
        job              := TOCRJob(TransymOCR1.NewJob);
        job.Filename     := IncludeTrailingPathDelimiter(DirectoryListBox1.Directory)+searchResult.Name;
        job.Finfo.ProcessOptions.SectioningOn := True;
        item             := Listview1.Items.Add;
        item.Caption     := ExtractFilename(job.Filename) +' ' + 'Queued';
        job.ExtraObject  := item;
        job.Start;
      end;
  until FindNext(searchResult) <> 0;

 finally
    // Must free up resources used by these successful finds
   FindClose(searchResult);
 end;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  TransymOCR1.OnOCRCompleted    := TransymOCR1OCRCompleted;
  TransymOCR1.OnOCRStatusUpdate := TransymOCR1OCRStatusUpdate;
  TransymOCR1.Start;
end;

procedure TForm4.FormResize(Sender: TObject);
begin

  Memo1.Left      := DirectoryListBox1.Left+DirectoryListBox1.Width+20; //   Button2.Left+Button2.Width + 20;
  ListView1.Left  := Memo1.Left;
  Memo1.Width     := ClientWidth - Memo1.Left;
  ListView1.Width := Memo1.Width;
  Memo1.Top       := ListView1.Height+20;
  Memo1.Height    := ClientHeight-ListView1.Top-ListView1.Height-StatusBar1.Height-20;
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  Memo1.Left      := DirectoryListBox1.Left+DirectoryListBox1.Width+20;
  ListView1.Left  := Memo1.Left;
  Memo1.Width     := ClientWidth - Memo1.Left;
  ListView1.Width := Memo1.Width;
  Memo1.Top       := ListView1.Height+20;
  Memo1.Height    := ClientHeight-ListView1.Top-ListView1.Height-StatusBar1.Height-20;
end;

procedure TForm4.TransymOCR1OCRCompleted(var job:IOCRJob);
var
  jjob      : TOCRJob;
  I         : Integer;
  ocrString : String;
  lines     : TStringList;
begin
  jjob  := TOCRJob(job);
  lines := TStringList.Create;
  try
   Memo1.Lines.Add('Complete Called');
   Memo1.Lines.Add('Source Filename: '+job.Filename);
   ocrString := '';
   for I := 0 to jjob.OCRResultsHeader.NumItems-1 do
     begin
      if((jjob.OCRResultItems[i].OCRCha=13) or
         (jjob.OCRResultItems[i].OCRCha=10)) then
        begin
          lines.Add(ocrString);
          ocrString := '';
        end
      else
        begin
          ocrString := ocrString + Char(jjob.OCRResultItems[i].OCRCha);
        end;
     end;
     Memo1.Lines.AddStrings(lines);
     SendMessage(Memo1.Handle, EM_LINESCROLL, 0,Memo1.Lines.Count);
   if(assigned(job.ExtraObject)) then
     begin
       TListItem(job.ExtraObject).Caption := ExtractFilename(job.Filename) +' Complete';
     end;
  finally
    FreeAndNil(lines);
  end;
end;

procedure TForm4.TransymOCR1OCRStatusUpdate(var job:IOCRJob;
  JobStatus: Integer; Progress: Single);
begin
  if(assigned(job.ExtraObject)) then
    begin
      TListItem(job.ExtraObject).Caption := ExtractFilename(job.Filename) +' ' + IntToStr(Trunc(Progress*100))+'%';
    end;
end;

end.
