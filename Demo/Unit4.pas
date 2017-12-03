unit Unit4;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, transymocr, Vcl.StdCtrls,tocrdll,
  Vcl.ComCtrls,system.generics.collections, Vcl.FileCtrl,transym.int.job;

type
  TForm4 = class(TForm)
    Memo1: TMemo;
    TransymOCR: TTransymOCR;
    StatusBar1: TStatusBar;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button3: TButton;
    ListView1: TListView;
    DirectoryListBox1: TDirectoryListBox;
    procedure TransymOCR1OCRCompleted(var job:IOCRJob);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure TransymOCROCRStatusUpdate(var job:IOCRJob; JobStatus: Integer;
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

uses transym.job;

procedure TForm4.Button1Click(Sender: TObject);
var
  job  : IOCRJob;
  item : TListItem;
  jobdata :PTOCRJobInfo2;
begin
  if(Opendialog1.Execute) then
    begin
     job                                   := TransymOCR.NewJob;
     job.Filename                          := OpenDialog1.FileName;
     item := Listview1.Items.Add;
     item.Caption := ExtractFilename(job.Filename) +' ' + 'Queued';
     job.GetJobData(jobdata);
     jobData.ProcessOptions.SectioningOn := True;
     job.ExtraObject := item;
     job.Start;
    end;
end;

procedure TForm4.Button2Click(Sender: TObject);
var
  job  : IOCRJob;
  item : TListItem;
begin
  job             := TransymOCR.NewJob;
  job.Filename    := 'C:\Program Files (x86)\Transym\TOCR\Viewer\Samples\Sample.tif';
  item            := Listview1.Items.Add;
  item.Caption    := ExtractFilename(job.Filename) +' ' + 'Queued';
  job.ExtraObject := item;
  job.Start;
end;

procedure TForm4.Button3Click(Sender: TObject);
var
  job          : IOCRJob;
  item         : TListItem;
  searchResult : TSearchRec;
  jobdata      : PTOCRJobInfo2;
begin
  findfirst(IncludeTrailingPathDelimiter(DirectoryListBox1.Directory)+'*',faAnyFile,searchResult);
 try
  repeat
    if((UpperCase(ExtractFileExt(searchResult.Name))='.BMP') or
       (UpperCase(ExtractFileExt(searchResult.Name))='.TIF')) then
      begin
        job              := TransymOCR.NewJob;
        job.Filename     := IncludeTrailingPathDelimiter(DirectoryListBox1.Directory)+searchResult.Name;
        job.GetJobData(jobdata);
        jobdata.ProcessOptions.SectioningOn := True;
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
  TransymOCR.OnOCRCompleted    := TransymOCR1OCRCompleted;
  TransymOCR.OnOCRStatusUpdate := TransymOCROCRStatusUpdate;
  TransymOCR.Start;
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
  I         : Integer;
  ocrString : String;
  lines     : TStringList;
  results   : PTOCRResultsHeader;
  header    : PTOCRResultsHeader;
  items     : POCRResultsArray;
begin
  lines := TStringList.Create;
  try
   Memo1.Lines.Add('Complete Called');
   Memo1.Lines.Add('Source Filename: '+job.Filename);
   ocrString := '';
   job.GetResults3(header,items);

   // results := PTOCRResultsHeader(header);
 //  items := TOCRJob(job).FOCRResultItems;
  // header := TOCRJob(job).FOCRResultsHeader;
  // items := TOCRJob(job).FOCRResultItemsEx;
  // results := TOCRJob(job).FOCRResultsHeaderEx;
 //  results := header;

   for I := 0 to header.NumItems-1 do
     begin
      if((items[i].OCRCha=13) or
         (items[i].OCRCha=10)) then
        begin
          lines.Add(ocrString);
          ocrString := '';
        end
      else
        begin
          ocrString := ocrString + Char(Items[i].OCRCha);
        end;
     end;
     Memo1.Lines.AddStrings(lines);
     Memo1.Lines.Add(ocrString);
     SendMessage(Memo1.Handle, EM_LINESCROLL, 0,Memo1.Lines.Count);
   if(assigned(job.ExtraObject)) then
     begin
       TListItem(job.ExtraObject).Caption := ExtractFilename(job.Filename) +' Complete';
     end;
  finally
    FreeAndNil(lines);
  end;
end;

procedure TForm4.TransymOCROCRStatusUpdate(var job:IOCRJob; JobStatus: Integer; Progress: Single);
begin
  if(assigned(job.ExtraObject)) then
    begin
      TListItem(job.ExtraObject).Caption := ExtractFilename(job.Filename) +' ' + IntToStr(Trunc(Progress*100))+'%';
    end;
end;

end.
