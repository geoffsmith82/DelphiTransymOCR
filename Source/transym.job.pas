unit transym.job;

interface

uses TOCRDll,transym.int.job,transymocr;

type
  TOCRJob = class(TInterfacedObject, IOCRJob)
    private
      Focr              : TTransymOCR;
      FstrfileName      : AnsiString;
      FOCRResults       : T_ByteArray;
      FJobNo            : Integer;
      FExtraObject      : TObject;
      Finfo             : TTOCRJobInfo2;
      FOCRResultsHeader : PTOCRResultsHeader;
      FOCRResultItems   : POCRResultsArray;
      function getFilename():AnsiString;
      procedure setFilename(const str:AnsiString);
      function getJobNo():Integer;
      procedure setJobNo(JobNoToSet:Integer);
      function getJobInfo(): TTOCRJobInfo2;
      procedure setJobInfo(infoToSet : TTOCRJobInfo2);
      function getObject():TObject;
      procedure setObject(obj:TObject);
    public
//      FOCRResultsHeaderEx : PTOCRResultsHeader;
//      FOCRResultItemsEx   : POCRResultsArrayEx;
      constructor Create(Sender: TTransymOCR);
      destructor Destroy(); override;
      procedure Start();
      function DoJob(): Integer;
      function Initialize():Integer;
      function Shutdown():Integer;
      procedure GetJobData(var info:PTOCRJobInfo2);
      function GetResultSize(var size:Integer):Integer;
      function GetJobStatus(var JobStatus:Integer;var Progress:Single;orient:Integer):Integer;
      procedure GetResults(mode:Integer = TOCRGetResults_NORMAL);
//      procedure GetResultsExInternal(mode:Integer = TOCRGetResults_NORMAL);
      procedure GetResults3(var results:PTOCRResultsHeader;var items:POCRResultsArray;mode:Integer = TOCRGetResults_NORMAL);
//      procedure GetResultsEX(var results:PTOCRResultsEx;var items:POCRResultsArrayEx;mode:Integer = TOCRGetResults_NORMAL);
    published
      property jobNo       : Integer read getJobNo write setJobNo;
      property Filename    : AnsiString read getFilename write setFilename;
      property ExtraObject : TObject read getObject write setObject;
  end;

implementation

uses windows,sysutils,TypInfo,RTTI;

function TOCRJob.Initialize():Integer;
begin
  OutputDebugString(PChar('OCRInitialise=Addr:'+IntToStr(Integer(@Self))));
  Result := TOCRInitialise(FjobNo);
  OutputDebugString(PChar('OCRInitialise='+IntToStr(Result)+'| JobNo:'+IntToStr(FjobNo)+'|Addr:'+IntToStr(Integer(@Self))));
end;

procedure TOCRJob.GetJobData(var info:PTOCRJobInfo2);
begin
  info := @FInfo;
end;

function TOCRJob.Shutdown():Integer;
begin
  if(FJobNo=-1) then
    begin
      OutputDebugString(PChar('-OCRShutDownAddr:'+IntToStr(Integer(@Self))));
      Result := -1;
      Exit;
    end;
  OutputDebugString(PChar('OCRShutDown=Addr:'+IntToStr(Integer(@Self))));
  Result := TOCRShutDown(FjobNo);
  OutputDebugString(PChar('OCRShutDown='+IntToStr(Result)+'JobNo:'+IntToStr(FjobNo)+'|Addr:'+IntToStr(Integer(@Self))));
end;


function TOCRJob.DoJob(): Integer;
begin
  OutputDebugString(PChar('TOCRDoJob2=Addr:'+IntToStr(Integer(@Self))));
  Result := TOCRDoJob2 (FjobNo,Finfo);
  OutputDebugString(PChar('TOCRDoJob2='+IntToStr(Result)+'JobNo:'+IntToStr(FjobNo)+'|Addr:'+IntToStr(Integer(@Self))));
end;

function TOCRJob.GetResultSize(var size:Integer):Integer;
var
  OCRJobResultsInf : Integer;
  ret : Integer;
begin
  Result := TOCRGetJobResults(FjobNo, OCRJobResultsInf, nil);
  OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)+'|Addr:'+IntToStr(Integer(@Self))));
  size := OCRJobResultsInf;
end;

{
procedure TOCRJob.GetResultsExInternal(mode:Integer = TOCRGetResults_NORMAL);
var
  OCRJobResultsInf : Integer;
  ret : Integer;
begin
  //ret := TOCRGetJobResults(jobNo, OCRJobResultsInf, Nil);
  ret := GetResultSize(OCRJobResultsInf);
  OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)+'|Addr:'+IntToStr(Integer(@Self))));
 // SetLength(FOCRResultsEx, OCRJobResultsInf+1);
  ret := TOCRGetJobResultsEx(FjobNo,mode, OCRJobResultsInf, Pointer(@FOCRResults[0]));
  OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)+'|Addr:'+IntToStr(Integer(@Self))));
  FOCRResultsHeaderEx := @FOCRResults[0];
//  results :=@FOCRResults[0];
  OutputDebugString(PChar('NumItemsCount:'+IntToStr(FOCRResultsHeader.NumItems)+'|Addr:'+IntToStr(Integer(@Self))));
 // FOCRResultItemsEx   := POCRResultsArray(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
//  items := POCRResultsArray(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
end; }

procedure TOCRJob.GetResults(mode:Integer);
var
  OCRJobResultsInf : Integer;
  ret : Integer;
begin
  //ret := TOCRGetJobResults(jobNo, OCRJobResultsInf, Nil);
  ret := GetResultSize(OCRJobResultsInf);
  OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)+'|Addr:'+IntToStr(Integer(@Self))));
  SetLength(FOCRResults, OCRJobResultsInf+1);
  ret := TOCRGetJobResults(FjobNo,OCRJobResultsInf, @FOCRResults[0]);
  OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)+'|Addr:'+IntToStr(Integer(@Self))));
  FOCRResultsHeader := @FOCRResults[0];
//  results :=@FOCRResults[0];
  OutputDebugString(PChar('NumItemsCount:'+IntToStr(FOCRResultsHeader.NumItems)+'|Addr:'+IntToStr(Integer(@Self))));
  FOCRResultItems   := POCRResultsArray(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
//  FOCRResultItemsEx := POCRResultsArrayEx(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
//  items := POCRResultsArray(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
end;

procedure TOCRJob.GetResults3(var results:PTOCRResultsHeader;var items:POCRResultsArray;mode:Integer);
var
  OCRJobResultsInf : Integer;
  ret : Integer;
//  results : Integer;
  offset : Integer;
begin
  FOCRResultsHeader := @FOCRResults[0];
  results           := @FOCRResults[0];
//  OutputDebugString(PChar('header:'+IntToStr(Int64(@FOCRResults[0]))));
//  OutputDebugString(PChar('header:'+IntToStr(Int64(@(results.Header)))));
  FOCRResultItems   := POCRResultsArray(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
  items := FOCRResultItems;
//  items := POCRResultsArray(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
//  OutputDebugString(PChar('Items:'+IntToStr(Int64(items))));
//   OutputDebugString(PChar('Items:'+IntToStr(Int64(@results.Items))));
end;

{
procedure TOCRJob.GetResultsEX(var results:PTOCRResultsEx;var items:POCRResultsArrayEx;mode:Integer);
var
  OCRJobResultsInf : Integer;
  ret : Integer;
//  results : Integer;
  offset : Integer;
begin
  FOCRResultsHeader := @FOCRResults[0];
//  results           := @FOCRResults[0];
//  OutputDebugString(PChar('header:'+IntToStr(Int64(@FOCRResults[0]))));
//  OutputDebugString(PChar('header:'+IntToStr(Int64(@(results.Header)))));
  FOCRResultItemsEx   := POCRResultsArrayEx(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
  results := PTOCRResultsEx(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
//  items := POCRResultsArray(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
//  OutputDebugString(PChar('Items:'+IntToStr(Int64(items))));
//   OutputDebugString(PChar('Items:'+IntToStr(Int64(@results.Items))));
//   Offset := Int64(@results.Items);
end;  }


function TOCRJob.getFilename():AnsiString;
begin
  Result := Copy(FstrfileName,0,length(FstrfileName));
end;

procedure TOCRJob.setFilename(const str: AnsiString);
begin
  FstrfileName := str;
  Finfo.InputFile := PAnsiChar(FstrfileName);
  if ExtractFileExt(UpperCase(str))='.TIF' then
    begin
      Finfo.JobType  := TOCRJobType_TiffFile;
    end
  else if ExtractFileExt(UpperCase(str))='.BMP' then
    begin
      Finfo.JobType  := TOCRJobType_DibFile;
    end
  else if ExtractFileExt(UpperCase(str))='.PDF' then
    begin
      Finfo.JobType  := TOCRJobType_PdfFile;
    end;
end;

function TOCRJob.getJobNo():Integer;
begin
  Result := FJobNo;
end;

procedure TOCRJob.setJobNo(JobNoToSet:Integer);
begin
  FJobNo := JobNoToSet;
end;

function TOCRJob.getJobInfo(): TTOCRJobInfo2;
begin
  Result := FInfo;
end;

procedure TOCRJob.setJobInfo(infoToSet : TTOCRJobInfo2);
begin
  Finfo := infoToSet;
end;

destructor TOCRJob.Destroy();
begin
  inherited;
  OutputDebugString(PChar('TOCRJob'));
end;

constructor TOCRJob.Create(Sender:TTransymOCR);
var
  i : Integer;
begin
  Focr           := Sender;
  jobNo          := 0;
  Finfo.StructId := 0;
  Finfo.JobType  := 0;
  Finfo.hMMF     := nil;
  Finfo.PageNo   := 0;
  Finfo.ProcessOptions.StructId           := 0;
  Finfo.ProcessOptions.InvertWholePage    := False;
  Finfo.ProcessOptions.DeskewOff          := False;
  Finfo.ProcessOptions.Orientation        := 0;
  Finfo.ProcessOptions.NoiseRemoveOff     := False;
  Finfo.ProcessOptions.LineRemoveOff      := False;
  Finfo.ProcessOptions.DeshadeOff         := False;
  Finfo.ProcessOptions.InvertOff          := False;
  Finfo.ProcessOptions.SectioningOn       := False;
  Finfo.ProcessOptions.MergeBreakOff      := False;
  Finfo.ProcessOptions.LineRejectOff      := False;
  Finfo.ProcessOptions.CharacterRejectOff := False;
  Finfo.ProcessOptions.LexOff             := False;
  for i := 0 to 255 do Finfo.ProcessOptions.DisableCharacter[i] := False;
end;


function TOCRJob.GetJobStatus(var JobStatus:Integer;var Progress:Single;orient:Integer):Integer;
begin
  Result := TOCRGetJobStatusEx (FJobNo,JobStatus,Progress,orient);
end;


function TOCRJob.getObject():TObject;
begin
  Result := FExtraObject;
end;

procedure TOCRJob.setObject(obj:TObject);
begin
  FExtraObject := obj;
end;


procedure TOCRJob.Start();
var
  job : IOCRJob;
begin
  job := Self;
  Focr.processJob(job);
end;


end.
