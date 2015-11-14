unit transymocr;

interface

uses
  System.SysUtils, System.Classes,TOCRDll,system.generics.collections,
  vcl.dialogs,windows;

type
  TOnOCRComplete = procedure (job:PObject) of object;
  TOnOCRStatusUpdate = procedure (job:PTOCRJobInfo2;JobStatus:Integer;Progress:Single) of object;

  TOCRResultsArray = Array [0..0] of TTOCRResultsItem;
  POCRResultsArray = ^TOCRResultsArray;

  TOCRJob = class
    private
    FocrObj      : TObject;
    FstrfileName : array [0..255] of AnsiChar;
    FOCRResults  : T_ByteArray;
    constructor Create(Sender: TObject);
    function getFilename():AnsiString;
    procedure setFilename(str:AnsiString);
    public
    OCRResultsHeader : PTOCRResultsHeader;
    OCRResultItems   : POCRResultsArray;
    jobNo            : Integer;
    info             : TTOCRJobInfo2;
    procedure Start();
    published
      property Filename : AnsiString read getFilename write setFilename;
  end;

  TOCRThread = class (TThread)
  private
    FocrObj : TObject;
    FcurrentProcessing: TDictionary<Integer, TOCRJob>;
    procedure ScheduleJob();
    procedure UpdateStatus();
    procedure HandleCompletedJob(JobNo: Integer);
  public
    OnOCRComplete : TOnOCRComplete;
    OnOCRStatusUpdate : TOnOCRStatusUpdate;
    constructor Create(CreateSuspended: Boolean;Sender: TObject);
    procedure Execute; override;

  end;


  TTransymOCR = class(TComponent)
  private
    { Private declarations }
    FOnOCRCompleted : TOnOCRComplete;
    FWorker         : TOCRThread;
    FQueue          : TQueue<TOCRJob>;
    FRunningJobs    : Integer;
    FOnOCRStatusUpdate : TOnOCRStatusUpdate;
  protected
    { Protected declarations }
  public
    { Public declarations }
    function GetSlotCount():LongInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start();
    function NewJob():TOCRJob;
    function processJob(var job:TOCRJob): Boolean;
  published
    { Published declarations }
    property OnOCRCompleted : TOnOCRComplete read FOnOCRCompleted write FOnOCRCompleted;
    property OnOCRStatusUpdate : TOnOCRStatusUpdate read FOnOCRStatusUpdate write FOnOCRStatusUpdate;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Tyson Technology', [TTransymOCR]);
end;


procedure TOCRThread.UpdateStatus();
var
  i         : Integer;
  progress  : Single;
  orient    : Integer;
  JobNo     : Integer;
  JobStatus : Integer;
  ocr       : TTransymOCR;
  job       : TOCRJob;
begin
  ocr := TTransymOCR(FocrObj);
  for i := 0 to FcurrentProcessing.Count-1 do
    begin
      JobNo := i;
      job := FcurrentProcessing[i];
      TOCRGetJobStatusEx (JobNo,JobStatus,Progress,orient);
      Synchronize(procedure
         begin
           if(Assigned(ocr.FOnOCRStatusUpdate)) then
             begin
               ocr.FOnOCRStatusUpdate(@job.info,JobStatus,Progress);
             end;
         end
      );
    end;
end;

procedure TOCRThread.Execute;
var
  WaitAnyStatus     : LongInt;
  ret               : Integer;
  JobNo             : LongInt;
  maxConcurrentJobs : Integer;
  ocr               : TTransymOCR;
begin
  inherited;
  self.NameThreadForDebugging('OCR Thread');
  FcurrentProcessing := TDictionary<Integer,TOCRJob>.Create;
 try
  ocr         := TTransymOCR(FocrObj);
  maxConCurrentJobs := ocr.GetSlotCount;
  repeat
    begin

   // if(FcurrentProcessing.Count<maxConCurrentJobs) then
      begin
        ScheduleJob();
      end;

    if(FcurrentProcessing.Count=0) then
      begin
        Sleep(500);  // Wait half second before checking queue again
        continue;
      end;
     Sleep(300);
    UpdateStatus();
     Sleep(500);
    UpdateStatus();

     WaitAnyStatus := 0;
     JobNo         := 0;

     ret := TOCRWaitForAnyJob(WaitAnyStatus,JobNo);
     OutputDebugString(PChar('WaitForAnyJob='+IntToStr(ret)+' Status:'+IntToStr(WaitAnyStatus)+' JobNo:'+IntToStr(JobNo)));
     if(ret<>TOCR_Ok) then
       begin
         continue;
       end;
   {  if(WaitAnyStatus=TOCRWait_NoJobsFound) then // Shouldn't get here
       begin
         Sleep(500);   // Wait half second before checking queue again
         continue;
       end; }
 //     if(WaitAnyStatus<>TOCRWait_Ok) then continue;
    HandleCompletedJob(JobNo);
    end
  until Terminated;
 finally
   FreeAndNil(FcurrentProcessing);
 end;
end;

procedure TOCRThread.HandleCompletedJob(JobNo: Integer);
var
  job: TOCRJob;
  OCRJobResultsInf : Integer;
  ret : Integer;
  j : Integer;
begin
  job := nil;
  OutputDebugString(PChar('Handling Completed Job ['+IntToStr(JobNo)+']'));
  if (Assigned(OnOCRComplete)) then
    begin
     job := FcurrentProcessing.Items[jobNo];
     FcurrentProcessing.Remove(jobNo);
     ret := TOCRGetJobResults(jobNo, OCRJobResultsInf, Nil);
     OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)));
     SetLength(job.FOCRResults, OCRJobResultsInf+1);
     ret := TOCRGetJobResults(jobNo, OCRJobResultsInf, @job.FOCRResults[0]);
     OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)));
     job.OCRResultsHeader := @job.FOCRResults[0];
     OutputDebugString(PChar('NumItemsCount:'+IntToStr(job.OCRResultsHeader.NumItems)));
     job.OCRResultItems   := POCRResultsArray(Integer(@job.FOCRResults[0])+sizeof(TTOCRResultsHeader));
     for j := 0 to job.OCRResultsHeader.NumItems-1 do
       begin
         OutputDebugString(PChar('Char:' +Char(TTOCRResultsItem(job.OCRResultItems[j]).OCRCha)));
       end;

     Synchronize(procedure
      begin
        OnOCRComplete(PObject(job));
      end);
        if (Assigned(job)) then
          begin
            TOCRShutDown(job.jobNo);
          end;
    end;
end;

procedure TOCRThread.ScheduleJob();
var
  queueLength : Integer;
  ocr         : TTransymOCR;
  job         : TOCRJob;
  ret         : Integer;
begin
  ocr := TTransymOCR(FocrObj);
  job := nil;
  Synchronize(procedure
     begin
       queueLength := ocr.FQueue.Count;
       if(queueLength>0) then
          begin
            job := ocr.FQueue.Dequeue;
          end;
     end);
if(job<>nil) then
 begin
  ret := TOCRInitialise(job.jobNo);
  OutputDebugString(PChar('OCRInitialise='+IntToStr(ret)+'| JobNo:'+IntToStr(job.jobNo)));

  // need to remove jobNo from currentProcessing
  FcurrentProcessing.Remove(job.jobNo);
  FcurrentProcessing.Add(job.jobNo,job);

  ret := TOCRDoJob2 (job.jobNo,job.info);
  OutputDebugString(PChar('OCRDoJob2='+IntToStr(ret)+'| JobNo:'+IntToStr(job.jobNo)));
  if(ret<>TOCR_Ok) then
     begin
       ret := TOCRShutDown(job.jobNo);
       OutputDebugString(PChar('OCRShutDown='+IntToStr(ret)));
       FreeAndNil(job);
     end;
 end;
end;

function TOCRJob.getFilename():AnsiString;
begin
  Result := Copy(FstrfileName,0,length(FstrfileName));
end;

procedure TOCRJob.setFilename(str: AnsiString);
begin
  FillChar(FstrfileName, SizeOf(FstrfileName), 0);
  StrPCopy(FstrfileName, str+#0);
  info.InputFile := FstrfileName;
  if ExtractFileExt(UpperCase(str))='.TIF' Then
            info.JobType  := TOCRJobType_TiffFile
  else if ExtractFileExt(UpperCase(str))='.BMP' Then
            info.JobType  := TOCRJobType_DibFile;
end;

constructor TOCRJob.Create(Sender:TObject);
var
  i : Integer;
begin
  FocrObj := Sender;
  info.StructId := 0;
  info.JobType  := 0;
  info.hMMF     := nil;
  info.PageNo   := 0;
  info.ProcessOptions.StructId        := 0;
  info.ProcessOptions.InvertWholePage := False;
  info.ProcessOptions.DeskewOff       := False;
  info.ProcessOptions.Orientation     := 0;
  info.ProcessOptions.NoiseRemoveOff  := False;
  info.ProcessOptions.LineRemoveOff   := False;
  info.ProcessOptions.DeshadeOff      := False;
  info.ProcessOptions.InvertOff       := False;
  info.ProcessOptions.SectioningOn    := False;
  info.ProcessOptions.MergeBreakOff   := False;
  info.ProcessOptions.LineRejectOff   := False;
  info.ProcessOptions.CharacterRejectOff := False;
  info.ProcessOptions.LexOff             := False;
  for i := 0 to 255 do info.ProcessOptions.DisableCharacter[i] := False;
end;

procedure TOCRJob.Start();
begin
  TTransymOCR(FocrObj).processJob(self);
end;

constructor TOCRThread.Create(CreateSuspended: Boolean;Sender: TObject);
begin
  inherited Create(CreateSuspended);
  FocrObj := Sender;
end;

constructor TTransymOCR.Create(AOwner: TComponent);
begin
  inherited;
  FRunningJobs := 0;
  FQueue := TQueue<TOCRJob>.Create;
  FWorker := TOCRThread.Create(True,Self);
end;


function TTransymOCR.NewJob():TOCRJob;
begin
  Result := TOCRJob.Create(Self);
end;

function TTransymOCR.processJob(var job:TOCRJob):Boolean;
begin
  Result := False;
  FQueue.Enqueue(job);
end;

procedure TTransymOCR.Start();
begin
  FWorker.OnOCRComplete := FOnOCRCompleted;
  FWorker.Start;
end;

destructor TTransymOCR.Destroy;
begin
  inherited;
  FWorker.Terminate;
  FreeAndNil(FQueue);
end;

function TTransymOCR.GetSlotCount():LongInt;
var
  slotCount : LongInt;
  ret : Integer;
begin
  slotCount := 0;
  ret := TOCRGetJobDBInfo(slotCount);
  OutputDebugString(PChar('OCRGetJobDBInfo'+IntToStr(ret)+' slotCount:'+IntToStr(slotCount)));
  Result := slotCount;
end;

end.
