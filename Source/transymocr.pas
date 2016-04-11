unit transymocr;

interface

uses
  System.SysUtils, System.Classes,TOCRDll,system.generics.collections,
  vcl.dialogs,windows,SyncObjs;

type


  TOCRResultsArray = Array [0..0] of TTOCRResultsItem;
  POCRResultsArray = ^TOCRResultsArray;


  IOCRJob = interface
  ['{22749B12-EBF7-466C-8C67-AB73EF1D2A09}']
    function getJobNo():Integer;
    procedure setJobNo(JobNoToSet:Integer);
    function getFilename():AnsiString;
    procedure setFilename(str:AnsiString);
    procedure Start();
    function getJobInfo(): TTOCRJobInfo2;
    procedure setJobInfo(infoToSet : TTOCRJobInfo2);
    function getObject():TObject;
    procedure setObject(obj:TObject);
    function DoJob(): Integer;
    function Initialize():Integer;
    function Shutdown():Integer;
    function GetResultSize(var size:Integer):Integer;
    function GetJobStatus(var JobStatus:Integer;var Progress:Single;orient:Integer):Integer;
    procedure GetResults(mode:Integer = TOCRGetResults_NORMAL);
    property jobNo    : Integer read getJobNo write setJobNo;
    property info     : TTOCRJobInfo2 read getJobInfo write setJobInfo;
    property Filename : AnsiString read getFilename write setFilename;
    property ExtraObject   : TObject read getObject write setObject;
  end;

  TOnOCRComplete = procedure (var job:IOCRJob) of object;
  TOnOCRStatusUpdate = procedure (var job:IOCRJob;JobStatus:Integer;Progress:Single) of object;

  TTransymOCR = class(TComponent)
  private
    { Private declarations }
    FOnOCRCompleted : TOnOCRComplete;
    FQueueCS        : TCriticalSection;
    FWorker         : TThread;
    FQueue          : TQueue<IOCRJob>;
    FRunningJobs    : Integer;
    FUpdateSpeed    : Integer;
    FOnOCRStatusUpdate : TOnOCRStatusUpdate;
    procedure setUpdateSpeed(millisecs:Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    function GetSlotCount():LongInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start();
    function NewJob():IOCRJob;
    function processJob(var job:IOCRJob): Boolean;
  published
    { Published declarations }
    property UpdateSpeed : Integer read FUpdateSpeed write setUpdateSpeed default 200;
    property OnOCRCompleted : TOnOCRComplete read FOnOCRCompleted write FOnOCRCompleted;
    property OnOCRStatusUpdate : TOnOCRStatusUpdate read FOnOCRStatusUpdate write FOnOCRStatusUpdate;
  end;


  TOCRJob = class(TInterfacedObject, IOCRJob)
    private
      Focr             : TTransymOCR;
      FstrfileName     : array [0..255] of AnsiChar;
      FOCRResults      : T_ByteArray;
      FJobNo           : Integer;
      FExtraObject     : TObject;
      constructor Create(Sender: TTransymOCR);
      function getFilename():AnsiString;
      procedure setFilename(str:AnsiString);
      function getJobNo():Integer;
      procedure setJobNo(JobNoToSet:Integer);
      function getJobInfo(): TTOCRJobInfo2;
      procedure setJobInfo(infoToSet : TTOCRJobInfo2);
      function getObject():TObject;
      procedure setObject(obj:TObject);
    public
      Finfo            : TTOCRJobInfo2;
      OCRResultsHeader : PTOCRResultsHeader;
      OCRResultItems   : POCRResultsArray;
      procedure Start();
      function DoJob(): Integer;
      function Initialize():Integer;
      function Shutdown():Integer;
      function GetResultSize(var size:Integer):Integer;
      function GetJobStatus(var JobStatus:Integer;var Progress:Single;orient:Integer):Integer;
      procedure GetResults(mode:Integer = TOCRGetResults_NORMAL);
    published
      property info        : TTOCRJobInfo2 read getJobInfo write setJobInfo;
      property jobNo       : Integer read getJobNo write setJobNo;
      property Filename    : AnsiString read getFilename write setFilename;
      property ExtraObject : TObject read getObject write setObject;
  end;

  TOCRThread = class (TThread)
  private
    Focr : TTransymOCR;
    FcurrentProcessing: TDictionary<Integer, IOCRJob>;
    function ScheduleJob():Integer;
    function UpdateStatus(): Integer;
    procedure HandleCompletedJob(JobNo: Integer);
  public
    constructor Create(CreateSuspended: Boolean;Sender: TTransymOCR);
    procedure Execute; override;
  end;

procedure Register;

implementation

uses math;

procedure Register;
begin
  RegisterComponents('Tyson Technology', [TTransymOCR]);
end;

function TOCRThread.UpdateStatus():Integer;
var
  progress  : Single;
  orient    : Integer;
  JobNo     : Integer;
  JobStatus : Integer;
  job       : IOCRJob;
  key       : Integer;
  value     : Integer;
begin
  Result := 0;
  value := 0;
  for key in FcurrentProcessing.Keys do
    begin
      job := TOCRJob(FcurrentProcessing[key]);
      if(job=nil) then continue;
      job.GetJobStatus(jobStatus,Progress,orient);
      if(Terminated) then
        begin
          Result := -1;
          exit;
        end;
      Synchronize(procedure
         begin
           if(Assigned(Focr.FOnOCRStatusUpdate)) then
             begin
      if(Terminated) then
        begin
          exit;
        end;

               Focr.FOnOCRStatusUpdate(job,JobStatus,Progress);
               value := max(value,Trunc(Progress*100));
             end;
         end
      );
    end;
  Result := value;
end;

procedure TOCRThread.Execute;
var
  WaitAnyStatus     : LongInt;
  ret               : Integer;
  JobNo             : LongInt;
  maxConcurrentJobs : Integer;
  queueLength       : Integer;
  key               : Integer;
  parameter         : Integer;
  saveValue,value   : Integer;
  JobNoSet          : Integer;
  scheduleRet       : Integer;
begin
  inherited;
try
  self.NameThreadForDebugging('OCR Thread');
  FcurrentProcessing := TDictionary<Integer,IOCRJob>.Create;

   JobNoSet  := TOCRConfig_DefaultJob;
   parameter := 0;//TOCRConfig_Dll_Errormode;
   value     := TOCRErrorMode_None;

   TOCRSetErrorMode(JobNoSet,0);
   OutputDebugString(PChar('TOCRSetConfig='+IntToStr(ret)));

 try
  maxConCurrentJobs := Focr.GetSlotCount;
  repeat
    begin
    if(Terminated) then break;

    scheduleRet := ScheduleJob();

    if(FcurrentProcessing.Count=0) then
      begin
        Sleep(500);  // Wait half second before checking queue again
        continue;
      end;

      Focr.FQueueCS.Enter;
       queueLength := Focr.FQueue.Count;
      Focr.FQueueCS.Leave;
    if((queueLength>0) and (scheduleRet=0)) then
      begin
        continue;
      end;

     WaitAnyStatus := 0;
     JobNo         := 0;

     repeat
       if(Terminated) then break;
       Sleep(Focr.UpdateSpeed);
     until UpdateStatus()>95;

     ret := TOCRWaitForAnyJob(WaitAnyStatus,JobNo);
     OutputDebugString(PChar('WaitForAnyJob='+IntToStr(ret)+' Status:'+IntToStr(WaitAnyStatus)+' JobNo:'+IntToStr(JobNo)));
     if(ret<>TOCR_Ok) then
       begin
         continue;
       end;
     if(WaitAnyStatus=TOCRWait_NoJobsFound) then // means all jobs have finished
       begin
         if(FcurrentProcessing.Count>0) then
           begin
             if(UpdateStatus()=100) then
               begin
               for key in FcurrentProcessing.Keys do
                 begin
                   HandleCompletedJob(FcurrentProcessing[Key].JobNo);
                 end;
               end;
           end;
       //  Sleep(500);   // Wait half second before checking queue again
         continue;
       end;
 //     if(WaitAnyStatus<>TOCRWait_Ok) then continue;
    HandleCompletedJob(JobNo);
    end
  until Terminated;
 finally
   FreeAndNil(FcurrentProcessing);
 end;
finally
  OutputDebugString(PChar('Thread Ending'));
end;
end;

procedure TOCRThread.HandleCompletedJob(JobNo: Integer);
var
  job    : TOCRJob;
  intJob : IOCRJob;
  OCRJobResultsInf : Integer;
  ret : Integer;
begin
  job := nil;
  OutputDebugString(PChar('Handling Completed Job ['+IntToStr(JobNo)+']'));
  if (Assigned(Focr.FOnOCRCompleted)) then
    begin
      job := TOCRJob(FcurrentProcessing.Items[jobNo]);
      job.GetResults;
      Synchronize(procedure
      begin
      if(Terminated) then
        begin
//          Result := -1;
          exit;
        end;

        intJob := FcurrentProcessing.Items[jobNo];
        Focr.FOnOCRCompleted(intJob);
      end);
     FcurrentProcessing.Remove(jobNo);
        if (Assigned(job)) then
          begin
            job.Shutdown;
          end;
    end;
end;

function TOCRThread.ScheduleJob():Integer;
var
  queueLength : Integer;
  job         : TOCRJob;
  ret         : Integer;
  parameter   : Integer;
  value       : Integer;
  JobNoSet    : Integer;
begin
  Result := TOCR_Ok;
  job := nil;
  Focr.FQueueCS.Enter;
       queueLength := Focr.FQueue.Count;
       if(queueLength>0) then
          begin
            job := TOCRJob(Focr.FQueue.Peek);
          end;

if(job<>nil) then
 begin
   ret := job.Initialize;//    TOCRInitialise(jobNo);
   if(ret=0) then // Success
     begin
         Focr.FQueue.Dequeue;
     end;
   Focr.FQueueCS.Leave;

   if(Ret=TOCRErr_NoFreeJobSlots) then
     begin
       job.Shutdown;
       Result := TOCRErr_NoFreeJobSlots;
       Exit;
     end;
   // need to remove jobNo from currentProcessing
//   FcurrentProcessing.Remove(job.jobNo);
   FcurrentProcessing.Add(job.jobNo,job);

   ret := job.DoJob();// TOCRDoJob2 (job.jobNo,job.Finfo);
   OutputDebugString(PChar('OCRDoJob2='+IntToStr(ret)+'| JobNo:'+IntToStr(job.jobNo)));
   if(ret<>TOCR_Ok) then
     begin
       ret := job.Shutdown;
       //TOCRShutDown(job.jobNo);
       FreeAndNil(job);
     end;
 end
else
  begin
       Focr.FQueueCS.Leave;
  end;
end;

function TOCRJob.Initialize():Integer;
begin
 OutputDebugString(PChar('OCRInitialise=Addr:'+IntToStr(Integer(@Self))));
 Result := TOCRInitialise(FjobNo);
 OutputDebugString(PChar('OCRInitialise='+IntToStr(Result)+'| JobNo:'+IntToStr(FjobNo)+'|Addr:'+IntToStr(Integer(@Self))));
end;

function TOCRJob.Shutdown():Integer;
begin
if(FJobNo=-1) then
  begin
   OutputDebugString(PChar('-OCRShutDownAddr:'+IntToStr(Integer(@Self))));
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
  Result := TOCRGetJobResults(FjobNo, OCRJobResultsInf, Nil);
  OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)+'|Addr:'+IntToStr(Integer(@Self))));
  size := OCRJobResultsInf;
end;

procedure TOCRJob.GetResults(mode:Integer);
var
  OCRJobResultsInf : Integer;
  ret : Integer;
begin
  //ret := TOCRGetJobResults(jobNo, OCRJobResultsInf, Nil);
  ret := GetResultSize(OCRJobResultsInf);
  OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)+'|Addr:'+IntToStr(Integer(@Self))));
  SetLength(FOCRResults, OCRJobResultsInf+1);
  ret := TOCRGetJobResults(FjobNo, OCRJobResultsInf, @FOCRResults[0]);
  OutputDebugString(PChar('OCRGetJobResults='+IntToStr(ret)+'| Size:'+IntToStr(OCRJobResultsInf)+'|Addr:'+IntToStr(Integer(@Self))));
  OCRResultsHeader := @FOCRResults[0];
  OutputDebugString(PChar('NumItemsCount:'+IntToStr(OCRResultsHeader.NumItems)+'|Addr:'+IntToStr(Integer(@Self))));
  OCRResultItems   := POCRResultsArray(Integer(@FOCRResults[0])+sizeof(TTOCRResultsHeader));
end;


function TOCRJob.getFilename():AnsiString;
begin
  Result := Copy(FstrfileName,0,length(FstrfileName));
end;

procedure TOCRJob.setFilename(str: AnsiString);
begin
  FillChar(FstrfileName, SizeOf(FstrfileName), 0);
  StrPCopy(FstrfileName, str+#0);
  Finfo.InputFile := FstrfileName;
  if ExtractFileExt(UpperCase(str))='.TIF' Then
            Finfo.JobType  := TOCRJobType_TiffFile
  else if ExtractFileExt(UpperCase(str))='.BMP' Then
            Finfo.JobType  := TOCRJobType_DibFile;
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

constructor TOCRThread.Create(CreateSuspended: Boolean;Sender: TTransymOCR);
begin
  inherited Create(CreateSuspended);
  Focr := Sender;
end;

constructor TTransymOCR.Create(AOwner: TComponent);
begin
  inherited;
  FRunningJobs := 0;
  FQueueCS     := TCriticalSection.Create;
  FQueue       := TQueue<IOCRJob>.Create;
  FWorker      := TOCRThread.Create(True,Self);
end;


procedure TTransymOCR.setUpdateSpeed(millisecs:Integer);
begin
  // ensure update speed is between .1 and 5 seconds
  FUpdateSpeed := min(max(100,millisecs),5000);
end;

function TTransymOCR.NewJob():IOCRJob;
begin
  Result := TOCRJob.Create(Self);
end;

function TTransymOCR.processJob(var job:IOCRJob):Boolean;
begin
  Result := False;
  FQueueCS.Enter;
  FQueue.Enqueue(job);
  FQueueCS.Leave;
end;

procedure TTransymOCR.Start();
var
  slotCount : Integer;
begin
  slotCount := 2;
  TOCRThread(FWorker).Start;
end;

destructor TTransymOCR.Destroy;
var
  slotCount : Integer;
begin
  inherited;
  slotCount := 2;
  FWorker.FreeOnTerminate := False;
  FWorker.Terminate;
  FWorker.WaitFor;
  FreeAndNil(FWorker);
if(FQueue.Count>0) then
 begin
  repeat
    FQueue.Dequeue;
  until FQueue.Count=0;
 end;
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
  Result := 2;
end;

end.
