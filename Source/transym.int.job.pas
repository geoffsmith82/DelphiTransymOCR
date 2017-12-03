unit transym.int.job;

interface

uses tocrdll;

type
  IOCRJob = interface
  ['{22749B12-EBF7-466C-8C67-AB73EF1D2A09}']
    function getJobNo():Integer;
    procedure setJobNo(JobNoToSet:Integer);
    function getFilename():AnsiString;
    procedure setFilename(const str:AnsiString);
    function getObject():TObject;
    procedure setObject(obj:TObject);
    function DoJob(): Integer;
    function Initialize():Integer;
    function Shutdown():Integer;
    procedure GetJobData(var info:PTOCRJobInfo2);
    function GetResultSize(var size:Integer):Integer;
    function GetJobStatus(var JobStatus:Integer;var Progress:Single;orient:Integer):Integer;
    procedure GetResults(mode:Integer = TOCRGetResults_NORMAL);
//    procedure GetResultsExInternal(mode:Integer = TOCRGetResults_NORMAL);
    procedure GetResults3(var results:PTOCRResultsHeader;var items:POCRResultsArray;mode:Integer = TOCRGetResults_NORMAL);
//    procedure GetResultsEX(var results:PTOCRResultsEx;var items:POCRResultsArrayEx;mode:Integer = TOCRGetResults_NORMAL);
    property jobNo    : Integer read getJobNo write setJobNo;
    property Filename : AnsiString read getFilename write setFilename;
    property ExtraObject   : TObject read getObject write setObject;
  end;

implementation

end.
