unit transymocr;

interface

uses
  System.SysUtils,
  System.Classes,
  TOCRDll,
  system.generics.collections,
  windows,
  SyncObjs,
  transym.int.job
  ;

type

  TOnOCRComplete = procedure (var job:IOCRJob) of object;
  TOnOCRStatusUpdate = procedure (var job:IOCRJob;JobStatus:Integer;Progress:Single) of object;

  TOCRStatusObject = record
     progress : Single;
     jobStatus : Integer;
     job : IOCRJob;
  end;

type
  TOCRThread = class (TThread)
  private
    procedure doOCRStatusUpdate;
    procedure doOCRCompleted;
    function GetOnOCRCompleted: TOnOCRComplete;
    procedure SetOnOCRCompleted(const Value: TOnOCRComplete);
    procedure SetOnOCRStatusUpdate(const Value: TOnOCRStatusUpdate);
    function GetOnOCRStatusUpdate: TOnOCRStatusUpdate;
    procedure ProcessMultipleCompletedJobs;
  strict protected
    FStatus   : TOCRStatusObject;
    FintJob   : IOCRJob;
    FSaveList : TList<IOCRJob>;
    FOnOCRCompleted : TOnOCRComplete;
    FOnOCRStatusUpdate : TOnOCRStatusUpdate;
    FinQueue: TThreadedQueue<IOCRJob>;
    FUpdateSpeed : Cardinal;
    FSignalNewFileEvent : TEvent;
    FcurrentProcessing: TDictionary<Integer, IOCRJob>;
    function ScheduleJob():Integer;
    function UpdateStatus(): Integer;
    procedure HandleCompletedJob(JobNo: Integer);
  public
    property OnOCRCompleted : TOnOCRComplete read GetOnOCRCompleted write SetOnOCRCompleted;
    property OnOCRStatusUpdate : TOnOCRStatusUpdate read GetOnOCRStatusUpdate write SetOnOCRStatusUpdate;
    procedure SignalNewFileEvent();
    procedure SetUpdateSpeed(iSpeed:Cardinal);
    function GetUpdateSpeed():Cardinal;
    constructor Create(inQueue: TThreadedQueue<IOCRJob>;inSaveList: TList<IOCRJob>;CreateSuspended: Boolean);
    destructor Destroy(); override;
    procedure Execute; override;
  end;

  TTransymOCR = class(TComponent)
  private
    { Private declarations }
    FWorker         : TOCRThread;
    FtodoQueue      : TThreadedQueue<IOCRJob>;
    FSaveList       : TList<IOCRJob>;
    FRunningJobs    : Integer;
    function GetOnOCRCompleted: TOnOCRComplete;
    function GetOnOCRStatusUpdate: TOnOCRStatusUpdate;
    procedure SetOnOCRCompleted(const Value: TOnOCRComplete);
    procedure SetOnOCRStatusUpdate(const Value: TOnOCRStatusUpdate);
  protected
    procedure setUpdateSpeed(millisecs:Integer);
    function GetUpdateSpeed: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start();
    function NewJob():IOCRJob;
    function processJob(var job:IOCRJob): Boolean;
  published
    { Published declarations }
    property UpdateSpeed : Integer read GetUpdateSpeed write setUpdateSpeed default 200;
    property OnOCRCompleted : TOnOCRComplete read GetOnOCRCompleted write SetOnOCRCompleted;
    property OnOCRStatusUpdate : TOnOCRStatusUpdate read GetOnOCRStatusUpdate write SetOnOCRStatusUpdate;
  end;


procedure Register;

implementation

uses math,
  transym.job,
  system.messaging
  ;

procedure Register;
begin
  RegisterComponents('Tyson Technology', [TTransymOCR]);
end;

procedure TOCRThread.SetOnOCRCompleted(const Value: TOnOCRComplete);
begin
  FOnOCRCompleted := Value;
end;

procedure TOCRThread.SetOnOCRStatusUpdate(const Value: TOnOCRStatusUpdate);
begin
  FOnOCRStatusUpdate := Value;
end;

procedure TOCRThread.SetUpdateSpeed(iSpeed:Cardinal);
begin
  // ensure update speed is between .1 and 5 seconds
  FUpdateSpeed := min(max(100,iSpeed),5000);
end;

function TOCRThread.UpdateStatus():Integer;
var
  JobStatus : Integer;
  progress  : Single;
  orient    : Integer;
  job       : IOCRJob;
  key       : Integer;
  value     : Integer;
begin
  Result := 0;
  value  := 0;
  for key in FcurrentProcessing.Keys do
    begin
      job := FcurrentProcessing[key];
      if(job=nil) then continue;
      job.GetJobStatus(jobStatus,Progress,orient);
      if(Terminated) then
        begin
          Result := -1;
          exit;
        end;
      FStatus.progress := Progress;
      FStatus.jobStatus := jobStatus;
      FStatus.job := job;
      value := max(value,Trunc(Progress*100)); // return the job with the highest completion status
      Synchronize(doOCRStatusUpdate);
    end;
  Result := value;
end;

procedure TOCRThread.SignalNewFileEvent();
begin
  FSignalNewFileEvent.SetEvent;
end;

procedure TOCRThread.Execute;
var
  WaitAnyStatus     : LongInt;
  ret               : Integer;
  JobNo             : LongInt;
  queueLength       : Integer;
  JobNoSet          : Integer;
  scheduleRet       : Integer;
begin
try
  inherited;
  NameThreadForDebugging('OCR Thread');
  JobNoSet  := TOCRConfig_DefaultJob;
  TOCRSetErrorMode(JobNoSet,0);
  OutputDebugString(PChar('TOCRSetConfig='+IntToStr(ret)));

  repeat
    begin
  //  try
    if(Terminated) then break;

    scheduleRet := ScheduleJob();

    if(FcurrentProcessing.Count=0) then
      begin
        FSignalNewFileEvent.WaitFor();
        continue;
      end;

    queueLength := FinQueue.TotalItemsPushed-FinQueue.TotalItemsPopped;
    if((queueLength>0) and (scheduleRet=0)) then
      begin
        continue;
      end;

    WaitAnyStatus := 0;
    JobNo         := 0;

    repeat
      if(Terminated) then break;
      Sleep(FUpdateSpeed);
    until UpdateStatus()>95;

    ret := TOCRWaitForAnyJob(WaitAnyStatus,JobNo);
    OutputDebugString(PChar('WaitForAnyJob='+IntToStr(ret)+' Status:'+IntToStr(WaitAnyStatus)+' JobNo:'+IntToStr(JobNo)));
    if(ret<>TOCR_Ok) then
      begin
        continue;
      end
    else if(WaitAnyStatus=TOCRWait_NoJobsFound) then // means all jobs have finished
      begin
        ProcessMultipleCompletedJobs;
      end
    else
      begin
        HandleCompletedJob(JobNo);
      end;
    end;
  until Terminated;
finally
  OutputDebugString(PChar('Thread Ending'));
end;
end;

procedure TOCRThread.ProcessMultipleCompletedJobs;
var
  key: Integer;
begin
  if (FcurrentProcessing.Count > 0) then
  begin
    if (UpdateStatus = 100) then
    begin
      for key in FcurrentProcessing.Keys do
      begin
        HandleCompletedJob(FcurrentProcessing[Key].JobNo);
      end;
    end;
  end;
end;

procedure TOCRThread.doOCRCompleted;
var
  ret : Integer;
begin
  if Terminated then Exit;
  if(not Assigned(FOnOCRCompleted)) then Exit;

  FOnOCRCompleted(FintJob);
  ret := FSaveList.Remove(FintJob);
  OutputDebugString(PChar('Removal Index:'+IntToStr(ret)));
end;

procedure TOCRThread.doOCRStatusUpdate;
begin
  if Terminated then Exit;
  if not Assigned(FOnOCRStatusUpdate) then Exit;

  FOnOCRStatusUpdate(Fstatus.job, FStatus.jobStatus, FStatus.progress);
end;

function TOCRThread.GetOnOCRCompleted: TOnOCRComplete;
begin
  Result := FOnOCRCompleted;
end;

function TOCRThread.GetOnOCRStatusUpdate: TOnOCRStatusUpdate;
begin
  Result := FOnOCRStatusUpdate;
end;

function TOCRThread.GetUpdateSpeed: Cardinal;
begin
  Result := FUpdateSpeed;
end;

procedure TOCRThread.HandleCompletedJob(JobNo: Integer);
var
  job : IOCRJob;
  ret : Integer;
begin
  job := nil;
  OutputDebugString(PChar('Handling Completed Job ['+IntToStr(JobNo)+']'));
  if (Assigned(FOnOCRCompleted)) then
    begin
      job := FcurrentProcessing.Items[jobNo];
      job.GetResults(TOCRGetResults_EXTENDED);
      FintJob := FcurrentProcessing.Items[jobNo];
      Synchronize(doOCRCompleted);
     FcurrentProcessing.Remove(jobNo);
     if (Assigned(job)) then
       begin
         job.Shutdown;
       end;
    end;
end;


function TOCRThread.ScheduleJob():Integer;
var
  job         : IOCRJob;
  ret         : Integer;
begin
  Result := TOCR_Ok;
  job := FinQueue.PopItem;
  if(job=nil) then
    begin
      Result := 0;
      Exit;
    end;
  ret := job.Initialize;//    TOCRInitialise(jobNo);

  if(Ret=TOCRErr_NoFreeJobSlots) then
    begin
      job.Shutdown;
      Result := TOCRErr_NoFreeJobSlots;
      Exit;
    end;

  FcurrentProcessing.Add(job.jobNo,job);

  ret := job.DoJob();// TOCRDoJob2 (job.jobNo,job.Finfo);
  OutputDebugString(PChar('OCRDoJob2='+IntToStr(ret)+'| JobNo:'+IntToStr(job.jobNo)));
  if(ret<>TOCR_Ok) then
     begin
       ret := job.Shutdown;
       FreeAndNil(job);
     end;
end;

destructor TOCRThread.Destroy();
begin
  FreeAndNil(FSignalNewFileEvent);
  FreeAndNil(FcurrentProcessing);
  inherited;
end;

constructor TOCRThread.Create(inQueue: TThreadedQueue<IOCRJob>;inSaveList: TList<IOCRJob>;CreateSuspended: Boolean);
begin
  FSignalNewFileEvent := TEvent.Create(nil,False,False,'OCRSignal');
  FcurrentProcessing := TDictionary<Integer,IOCRJob>.Create;
  FinQueue := inQueue;
  FSaveList := inSaveList;
  inherited Create(CreateSuspended);
  FreeOnTerminate := False;
end;

constructor TTransymOCR.Create(AOwner: TComponent);
begin
  inherited;
  FRunningJobs := 0;
  FtodoQueue   := TThreadedQueue<IOCRJob>.Create(100,0,INFINITE);;
  FSaveList    := TList<IOCRJob>.Create;
  FWorker      := TOCRThread.Create(FtodoQueue,FSaveList,True);
end;


procedure TTransymOCR.setUpdateSpeed(millisecs:Integer);
begin
  FWorker.SetUpdateSpeed(millisecs);
end;

function TTransymOCR.NewJob():IOCRJob;
begin
  Result := TOCRJob.Create();
  FSaveList.Add(Result);
end;

function TTransymOCR.processJob(var job:IOCRJob):Boolean;
begin
  Result := False;
  FtodoQueue.PushItem(job);
  FWorker.SignalNewFileEvent;
end;

procedure TTransymOCR.SetOnOCRCompleted(const Value: TOnOCRComplete);
begin
  FWorker.SetOnOCRCompleted(value);
end;

procedure TTransymOCR.SetOnOCRStatusUpdate(const Value: TOnOCRStatusUpdate);
begin
  FWorker.SetOnOCRStatusUpdate(Value);
end;

procedure TTransymOCR.Start();
begin
  FWorker.Start;
end;

destructor TTransymOCR.Destroy;
begin
  inherited;
  FWorker.Terminate;
  FtodoQueue.PushItem(nil);
  FWorker.SignalNewFileEvent;
  FWorker.WaitFor;
  FreeAndNil(FWorker);
  FreeAndNil(FtodoQueue);
  FreeAndNil(FSaveList);
end;

function TTransymOCR.GetOnOCRCompleted: TOnOCRComplete;
begin
  Result := FWorker.OnOCRCompleted;
end;

function TTransymOCR.GetOnOCRStatusUpdate: TOnOCRStatusUpdate;
begin
  Result := FWorker.OnOCRStatusUpdate;
end;

function TTransymOCR.GetUpdateSpeed: Integer;
begin
  Result := FWorker.GetUpdateSpeed;
end;

end.
