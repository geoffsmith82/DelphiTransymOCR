{———————————————————————————————————————————————————————————————}
{                                                               }
{       Borland Delphi Run-time Library                         }
{       Transym OCR API Interface Unit                          }
{                                                               }
{       Copyright © 2001 Transym Computer Services Ltd          }
{                                                               }
{       Translator:                                             }
{       2002-10-25 Olrik Larsen, Olrik@MercuryGroup.dk          }
{                                                               }
{       Last modified:                                          }
{       2002-10-29 Olrik Larsen, Olrik@MercuryGroup.dk          }
{                                                               }
{       Last modified:                                          }
{       2007-07-19 Sebastien Jallier, sj@eaistorage.com         }
{                                                               }
{       Last modified:                                          }
{       2015-11-10 Geoffrey Smith, geoff@tysontechnology.com.au }
{                                                               }
{———————————————————————————————————————————————————————————————}
unit TOCRDll;

{————— Compiler directives ———————————————————————————————————————————————————————————————————————————————————————————————————}
{$ALIGN 2} // Set align to be wordaligned, standard is ALIGN 8 for WinAPI, and ALIGN 1 for unknown API calls (same as Packed)
{$WEAKPACKAGEUNIT} // Couses this unit to be visible only at the moment of runtime inclusion and execution

interface

{————— Constant declarations —————————————————————————————————————————————————————————————————————————————————————————————————}
const
  TOCRJobMsgLength                    = 512;     // max length of a job status message

  TOCRMaxPPM                          = 78740;   // max pixels per meter
  TOCRMinPPM                          = 984;     // min pixels per metre

  TOCRConfig_DefaultJob               = -1;
  TOCRConfig_Dll_Errormode            = 0;

  // Setting for JobNo for TOCRSetErrorMode and TOCRGetErrorMode
  TOCRDefErrorMode                    = -1;      // set/get the API error mode for all jobs

  // Settings for ErrorMode for TOCRSetErrorMode and TOCRGetErrorMode
  TOCRErrorMode_None                  = 0;       // API errors unseen (use return status of API calls)
  TOCRErrorMode_Silent                = 0;       // API errors unseen (use return status of API calls)
  TOCRErrorMode_MsgBox                = 1;       // API errors will bring up a message box

  // Setting for TOCRShutdown
  TOCRShutdownAll                     = -1;      // stop and shutdown processing for all jobs

  // Values returnd by TOCRGetJobStatus JobStatus
  TOCRJobStatus_Error                 = -1;      // an error ocurred processing the last job
  TOCRJobStatus_Busy                  = 0;       // the job is still processing
  TOCRJobStatus_Done                  = 1;       // the job completed successfully
  TOCRJobStatus_Idle                  = 2;       // no job has been specified yet

  // Settings for TOCRJOBINFO.JobType
  TOCRJobType_TiffFile                = 0;       // TOCRJOBINFO.InputFile specifies a tiff file
  TOCRJobType_DibFile                 = 1;       // TOCRJOBINFO.InputFile specifies a dib (bmp) file
  TOCRJobType_DibClipBoard            = 2;       // clipboard contains a dib (clipboard format CF_DIB)
  TOCRJobType_MMFileHandle            = 3;       // TOCRJOBINFO.PageNo specifies a handle to a memory mapped DIB file

  // Settings for TOCRJOBINFO.Orientation
  TOCRJobOrient_Auto                  = 0;       // detect orientation and rotate automatically
  TOCRJobOrient_Off                   = 255;     // don't rotate
  TOCRJobOrient_90                    = 1;       // 90 degrees clockwise rotation
  TOCRJobOrient_180                   = 2;       // 180 degrees clockwise rotation
  TOCRJobOrient_270                   = 3;       // 270 degrees clockwise rotation

  // Values returned by TOCRGetJobDBInfo
  TOCRJobSlot_Free                    = 0;       // job slot is free for use
  TOCRJobSlot_OwnedByYou              = 1;       // job slot is in use by your process
  TOCRJobSlot_BlockedByYou            = 2;       // blocked by own process (re-initialise)
  TOCRJobSlot_OwnedByOther            = -1;      // job slot is in use by another process (can't use)
  TOCRJobSlot_BlockedByOther          = -2;      // blocked by another process (can't use)

  // Values returned in WaitAnyStatus by TOCRWaitForAnyJob
  TOCRWait_Ok                         = 0;       // JobNo is the job that finished (get and check it's JobStatus)
  TOCRWait_ServiceAbort               = 1;       // JobNo is the job that failed (re-initialise)
  TOCRWait_ConnectionBroken           = 2;       // JobNo is the job that failed (re-initialise)
  TOCRWait_Failed                     = -1;      // JobNo not set - check manually
  TOCRWait_NoJobsFound                = -2;      // JobNo not set - no running jobs found

  // values for Mode for TOCRGetJobResultsEx
  TOCRGetResults_NORMAL               = 0;       // return results for TOCRRESULTS
  TOCRGetResults_EXTENDED             = 1;       // return results for TOCRRESULTSEX

  // Values returned in ResultsInf by TOCRGetJobResults
  TOCRGetResults_NoResults            = -1;      // no results are available

  // Values for TOCRConvertFormat InputFormat
  TOCRCONVERTFORMAT_TIFFFILE          = TOCRJOBTYPE_TIFFFILE;

  // Values for TOCRConvertFormat OutputFormat
  TOCRCONVERTFORMAT_DIBFILE           = TOCRJOBTYPE_DIBFILE;
  TOCRCONVERTFORMAT_MMFILEHANDLE      = TOCRJOBTYPE_MMFILEHANDLE;

  // Values for licence features (returned by TOCRGetLicenceInfoEx)
  TOCRLICENCE_STANDARD                = 1;       // standard licence (no higher characters)
  TOCRLICENCE_EURO                    = 2;       // higher characters
  TOCRLICENCE_EUROUPGRADE             = 3;       // standard licence upgraded to euro

// List of all error codes, Version 1.3
  TOCR_Ok                             = 0;

  // Error codes returned by an API function
  TOCRErr_IllegalJobNo                = 1;
  TOCRErr_FailLockDB                  = 2;
  TOCRErr_NoFreeJobSlots              = 3;
  TOCRErr_FailStartService            = 4;
  TOCRErr_FailInitService             = 5;
  TOCRErr_JobSlotNotInit              = 6;
  TOCRErr_JobSlotInUse                = 7;
  TOCRErr_ServiceAbort                = 8;
  TOCRErr_ConnectionBroken            = 9;
  TOCRErr_InvalidStructId             = 10;
  TOCRErr_FailGetVersion              = 11;
  TOCRErr_FailLicenceInf              = 12;
  TOCRErr_LicenceExceeded             = 13;

  TOCRErr_FailGetJobStatus1           = 20;
  TOCRErr_FailGetJobStatus2           = 21;
  TOCRErr_FailGetJobStatus3           = 22;
  TOCRErr_FailConvTiff                = 23;

  TOCRErr_FailDoJob1                  = 30;
  TOCRErr_FailDoJob2                  = 31;
  TOCRErr_FailDoJob3                  = 32;
  TOCRErr_FailDoJob4                  = 33;
  TOCRErr_FailDoJob5                  = 34;
  TOCRErr_FailDoJob6                  = 35;
  TOCRErr_FailDoJob7                  = 36;
  TOCRErr_UnknownJobType1             = 40;
  TOCRErr_JobNotStarted1              = 41;
  TOCRErr_FailDupHandle               = 42;

  TOCRErr_FailGetJobStatusMsg1        = 45;
  TOCRErr_FailGetJobStatusMsg2        = 46;

  TOCRErr_FailGetNumPages1            = 50;
  TOCRErr_FailGetNumPages2            = 51;
  TOCRErr_FailGetNumPages3            = 52;
  TOCRErr_FailGetNumPages4            = 53;
  TOCRErr_FailGetNumPages5            = 54;

  TOCRErr_FailGetResults1             = 60;
  TOCRErr_FailGetResults2             = 61;
  TOCRErr_FailGetResults3             = 62;
  TOCRErr_FailGetResults4             = 63;
  TOCRErr_FailAllocMem100             = 64;
  TOCRErr_FailAllocMem101             = 65;
  TOCRErr_FileNotSpecified            = 66;
  TOCRErr_IunputNotSpecified          = 67;
  TOCRErr_OutputNotSpecified          = 68;

  TOCRErr_FailRotateBitmap            = 70;

  // Error codes which may be seen in a msgbox or console but will not be returned by an API function
  TOCRErr_InvalidServiceStart         = 1000;
  TOCRErr_FailServiceInit             = 1001;
  TOCRErr_FailLicence1                = 1002;
  TOCRErr_FailServiceStart            = 1003;
  TOCRErr_UnknownCmd                  = 1004;
  TOCRErr_FailReadCommand             = 1005;
  TOCRErr_FailReadOptions             = 1006;
  TOCRErr_FailWriteJobStatus1         = 1007;
  TOCRErr_FailWriteJobStatus2         = 1008;
  TOCRErr_FailWriteThreadH            = 1009;
  TOCRErr_FailReadJobInfo1            = 1010;
  TOCRErr_FailReadJobInfo2            = 1011;
  TOCRErr_FailReadJobInfo3            = 1012;
  TOCRErr_FailWriteProgress           = 1013;
  TOCRErr_FailWriteJobStatusMsg       = 1014;
  TOCRErr_FailWriteResultsSize        = 1015;
  TOCRErr_FailWriteResults            = 1016;
  TOCRErr_FailWriteAutoOrient         = 1017;
  TOCRErr_FailLicence2                = 1018;
  TOCRErr_FailLicence3                = 1019;

  TOCRErr_TooManyColumns              = 1020;
  TOCRErr_TooManyRows                 = 1021;
  TOCRErr_ExceededMaxZone             = 1022;
  TOCRErr_NStackTooSmall              = 1023;
  TOCRErr_AlgoErr1                    = 1024;
  TOCRErr_AlgoErr2                    = 1025;
  TOCRErr_ExceededMaxCP               = 1026;
  TOCRErr_CantFindPage                = 1027;
  TOCRErr_UnSupportedImageType        = 1028;
  TOCRErr_ImageTooWide                = 1029;
  TOCRErr_ImageTooLong                = 1030;
  TOCRErr_UnknownJobType2             = 1031;
  TOCRErr_TooWideToRot                = 1032;
  TOCRErr_TooLongToRot                = 1033;
  TOCRErr_InvalidPageNo               = 1034;
  TOCRErr_FailReadJobTypeNumBytes     = 1035;
  TOCRErr_FailReadFileName            = 1036;
  TOCRErr_FailSendNumPages            = 1037;
  TOCRErr_FailOpenClip                = 1038;
  TOCRErr_NoDIBOnClip                 = 1039;
  TOCRErr_FailReadDIBClip             = 1040;
  TOCRErr_FailLockDIBClip             = 1041;
  TOCRErr_UnkownDIBFormat             = 1042;
  TOCRErr_FailReadDIB                 = 1043;
  TOCRErr_NoXYPPM                     = 1044;
  TOCRErr_FailCreateDIB               = 1045;
  TOCRErr_FailWriteDIBClip            = 1046;
  TOCRErr_FailAllocMemDIB             = 1047;
  TOCRErr_FailLockMemDIB              = 1048;
  TOCRErr_FailCreateFile              = 1049;
  TOCRErr_FailOpenFile1               = 1050;
  TOCRErr_FailOpenFile2               = 1051;
  TOCRErr_FailOpenFile3               = 1052;
  TOCRErr_FailOpenFile4               = 1053;
  TOCRErr_FailReadFile1               = 1054;
  TOCRErr_FailReadFile2               = 1055;
  TOCRErr_FailFindData1               = 1056;
  TOCRErr_TiffError1                  = 1057;
  TOCRErr_TiffError2                  = 1058;
  TOCRErr_TiffError3                  = 1059;
  TOCRErr_TiffError4                  = 1060;
  TOCRErr_FailReadDIBHandle           = 1061;
  TOCRErr_PageTooBig                  = 1062;

  TOCRErr_FailReadFileName1           = 1070;
  TOCRErr_FailReadFileName2           = 1071;
  TOCRErr_FailReadFileName3           = 1072;
  TOCRErr_FailReadFileName4           = 1073;
  TOCRErr_FailReadFileName5           = 1074;

  TOCRErr_FailReadFormat1             = 1080;
  TOCRErr_FailReadFormat2             = 1081;

  TOCRErr_FailAllocMem1               = 1101;
  TOCRErr_FailAllocMem2               = 1102;
  TOCRErr_FailAllocMem3               = 1103;
  TOCRErr_FailAllocMem4               = 1104;
  TOCRErr_FailAllocMem5               = 1105;
  TOCRErr_FailAllocMem6               = 1106;
  TOCRErr_FailAllocMem7               = 1107;
  TOCRErr_FailAllocMem8               = 1108;
  TOCRErr_FailAllocMem9               = 1109;
  TOCRErr_FailAllocMem10              = 1110;

  TOCRErr_FailWriteMMFH               = 1150;
  TOCRErr_FailReadACK                 = 1151;
  TOCRErr_FailFileMap                 = 1152;
  TOCRErr_FailFileView                = 1153;

  TOCRErr_BufferOverFlow1             = 2001;

  TOCRErr_MapOverFlow                 = 2002;
  TOCRErr_ReBreakNextCall             = 2003;
  TOCRErr_ReBreakNextData             = 2004;
  TOCRErr_ReBreakExactCall            = 2005;
  TOCRErr_MaxZCanOverFlow1            = 2006;
  TOCRErr_MaxZCanOverFlow2            = 2007;
  TOCRErr_BufferOverFlow2             = 2008;
  TOCRErr_NumKcOverFlow               = 2009;
  TOCRErr_BufferoverFlow3             = 2010;
  TOCRErr_BufferOverFlow4             = 2011;
  TOCRErr_SeedError                   = 2012;

  TOCRErr_FCZYRef                     = 2020;
  TOCRErr_MaxTextLines1               = 2021;
  TOCRErr_LineIndex                   = 2022;
  TOCRErr_MaxFCZSOnLine               = 2023;
  TOCRErr_MemAlloc1                   = 2024;
  TOCRErr_MergeBreak                  = 2025;

  TOCRErr_DKernPRange1                = 2030;
  TOCRErr_DKernPRange2                = 2031;
  TOCRErr_BufferOverFlow5             = 2032;
  TOCRErr_BufferOverFlow6             = 2033;

  TOCRErr_FileOpen1                   = 2040;
  TOCRErr_FileOpen2                   = 2041;
  TOCRErr_FileOpen3                   = 2042;
  TOCRErr_FileRead1                   = 2043;
  TOCRErr_FileRead2                   = 2044;
  TOCRErr_SpwidZero                   = 2045;
  TOCRErr_FailAllocMemLex1            = 2046;
  TOCRErr_FailAllocMemLex2            = 2047;

  TOCRErr_BadObWidth                  = 2050;
  TOCRErr_BadRotation                 = 2051;

  TOCRErr_REJHIDMemAlloc              = 2055;
  TOCRErr_MatchBufferOverflow         = 2060;

  TOCRErr_UIdA                        = 2070;
  TOCRErr_UIdB                        = 2071;
  TOCRErr_ZeroUId                     = 2072;
  TOCRErr_Certaintydbnotinit          = 2073;
  TOCRErr_MemAllocIndex               = 2074;
  TOCRErr_CertainTYDB_Init            = 2075;
  TOCRErr_CertainTYDB_Delete          = 2076;
  TOCRErr_CertainTYDB_Insert1         = 2077;
  TOCRErr_CertainTYDB_Insert2         = 2078;
  TOCRErr_OpenXorNearest              = 2079;
  TOCRErr_XorNearset                  = 2079;

  TOCRErr_OpenSettings                = 2080;
  TOCRErr_ReadSettings1               = 2081;
  TOCRErr_ReadSettings2               = 2082;
  TOCRErr_BadSettings                 = 2083;
  TOCRErr_WriteSettings               = 2084;
  TOCRErr_MaxScoreDiff                = 2085;

  TOCRErr_YDimRefZero1                = 2090;
  TOCRErr_YDimRefZero2                = 2091;
  TOCRErr_YDimRefZero3                = 2092;
  TOCRErr_AsmFileOpen                 = 2093;
  TOCRErr_AsmFileRead                 = 2094;
  TOCRErr_MemAllocAsm                 = 2095;
  TOCRErr_MemReAllocAsm               = 2096;
  TOCRErr_SdbFileOpen                 = 2097;
  TOCRErr_SdbFileRead                 = 2098;
  TOCRErr_SdbFileBad1                 = 2099;
  TOCRErr_SdbFileBad2                 = 2100;
  TOCRErr_MemAllocSDB                 = 2101;
  TOCRErr_Devel1                      = 2102;
  TOCRErr_Devel2                      = 2103;
  TOCRErr_Devel3                      = 2104;
  TOCRErr_Devel4                      = 2105;
  TOCRErr_Devel5                      = 2106;
  TOCRErr_Devel6                      = 2107;
  TOCRErr_Devel7                      = 2108;
  TOCRErr_Devel8                      = 2109;
  TOCRErr_Devel9                      = 2110;
  TOCRErr_Devel10                     = 2111;
  TOCRErr_Devel11                     = 2112;
  TOCRErr_Devel12                     = 2113;
  TOCRErr_Devel13                     = 2114;
  TOCRErr_FileOpen4                   = 2115;
  TOCRErr_FileOpen5                   = 2116;
  TOCRErr_FileOpen6                   = 2117;
  TOCRErr_FileRead3                   = 2118;
  TOCRErr_FileRead4                   = 2119;
  TOCRErr_ZoomGTooBig                 = 2120;
  TOCRErr_ZoomGOutOfRange             = 2121;

  TOCRErr_Memallocresults             = 2130;

  TOCRErr_MemAllocHeap                = 2140;
  TOCRErr_HeapNotInitialised          = 2141;
  TOCRErr_MemLimitHeap                = 2142;
  TOCRErr_MemReAllocHeap              = 2143;
  TOCRErr_MemAllocFCZBM               = 2144;
  TOCRErr_FCZBMOverlap                = 2145;
  TOCRErr_FCZBMLocation               = 2146;
  TOCRErr_MemReAllocFCZBM             = 2147;
  TOCRErr_MemAllocFCHBM               = 2148;
  TOCRErr_MemReAllocFCHBM             = 2149;

{————— Type declarations —————————————————————————————————————————————————————————————————————————————————————————————————————}
Type
  PTOCRProcessOptions= ^TTOCRProcessOptions;
  TTOCRProcessOptions= Record
    StructId                    : LongInt;       // This should be 0, 1 or 2 and affects the interpretation of the X,Y data in the results.
    InvertWholePage             : WordBool;      // If this flag is set the "Job" will be inverted prior to processing.
    DeskewOff                   : WordBool;      // If this flag is set no attempt will be made to deskew the job.
    Orientation                 : Byte;          // This should be set to one of
                                                 //   TOCRJOBORIENT_AUTO  detect orientation and rotate automatically
                                                 //   TOCRJOBORIENT_OFF   don't rotate
                                                 //   TOCRJOBORIENT_90    90 degrees clockwise rotation
                                                 //   TOCRJOBORIENT_180   180 degrees clockwise rotation
                                                 //   TOCRJOBORIENT_270   270 degrees clockwise rotation
    NoiseRemoveOff              : WordBool;      // If this flag is set no attempt will be made to remove noise from the job.
    LineRemoveOff               : WordBool;      // If this flag is set no attempt will be made to remove lines from the job.
    DeshadeOff                  : WordBool;      // If this flag is set no attempt will be made to remove shading from the job.
    InvertOff                   : WordBool;      // If this flag is set individual zones in a job will not be analysed for auto inversion.
    SectioningOn                : WordBool;      // If this flag is set the page will be sectioned and results will be returned in "read order".
    MergeBreakOff               : WordBool;      // If this flag is set no attempt will be made to merge broken characters or break merged characters.
    LineRejectOff               : WordBool;      // If this flag is set no attempt will be made to lines of rubbish.
    CharacterRejectOff          : WordBool;      // If this flag is set no attempt will be made to reject characters that are very poor matches.
    LexOff                      : WordBool;      // If this flag is set the Lexicon will not be used to improve character recognition.
    DisableCharacter: Array[0..255] Of WordBool; // If any of these flags are set then the character with the respective number will not be considered when trying to find a match.
  End; // TTOCRProcessOptions
  PTOCRJobInfo=^TTOCRJobInfo;
  TTOCRJobInfo= Record
    StructId                    : LongInt;             // This should be set to 0.
    JobType                     : LongInt;             // This should be one of the TOCRJOBTYPE_ values.
    InputFile                   : PAnsiChar;{String;}      // If the JobType requires a file then set this value to the full path and file name for the file. Otherwise it can be ignored.
    PageNo                      : LongInt;             // The page number (dimensioned from 0) of the page to be processed in a multi-page TIFF file.
                                                       // Alternatively if the JobType is TOCRJOBTYPE_MMFILEHANDLE PageNo should be set to the handle of a memory mapped file containg a packed DIB.  (See Example4 in modXamples in the source).
    ProcessOptions              : TTOCRProcessOptions; // See description for TTOCRProcessOptions.
  End; // TTOCRJobInfo

  PTOCRJobInfo2=^TTOCRJobInfo2;
  TTOCRJobInfo2= record
    StructId                    : LongInt;             // This should be set to 0.
    JobType                     : LongInt;             // This should be one of the TOCRJOBTYPE_ values.
    InputFile                   : PAnsiChar;{String;}      // If the JobType requires a file then set this value to the full path and file name for the file. Otherwise it can be ignored.
    hMMF                        : PInteger;            // If the JobType is TOCRJOBTYPE_MMFILEHANDLE this should be set to the handle of a memory mapped file containg a packed DIB.  (See Example4 in modXamples in the source).
    PageNo                      : LongInt;             // The page number (dimensioned from 0) of the page to be processed in a multi-page TIFF file.
    ProcessOptions              : TTOCRProcessOptions; // See description for TTOCRProcessOptions.
  end; // TTOCRJobInfo


  PTOCRResultsHeader=^TTOCRResultsHeader;
  TTOCRResultsHeader= Record
    StructId                    : LongInt; // This should be 0.
    XPixelsPerInch              : LongInt; // Number of pixels per inch in the horizontal direction.
    YPixelsPerInch              : LongInt; // Number of pixels per inch in the vertical direction.
    NumItems                    : LongInt; // The number of TTOCRResultsItem structures to follow.
    MeanConfidence              : Single;  // The average confidence off all non-space characters.
                                           // See TTOCRResultsItem below for a discussion of Confidence.
  End; // TTOCRResultsHeader
  PTOCRResultsItem=^TTOCRResultsItem;
  TTOCRResultsItem= Record
    StructId                    : SmallInt; // This should be 0.
    OCRCha                      : SmallInt; // The OCR'd character number.
    Confidence                  : Single;   // Confidence in the accuracy of the character (for non-space characters only).
    XPos                        : SmallInt; // The horizontal position of the character in pixels.
    YPos                        : SmallInt; // The vertical position of the character in pixels.
    XDim                        : SmallInt; // The width of the character in pixels.
    YDim                        : SmallInt; // The height of the character in pixels.
  End; // TTOCRResultsItem
  PTOCRResults=^TTOCRResults;
  TTOCRResults= Record
    Header                      : TTOCRResultsHeader;        // See description for TTOCRResultsHeader
    Items                       : Array Of TTOCRResultsItem; // See description for TTOCRResultsItem
  End; // TTOCRResults
  PTOCRResultsItemExAlt=^TTOCRResultsItemExAlt;
  TTOCRResultsItemExAlt= Record
    Valid 			: SmallInt; // Will be 1 if the remaining items in the structure contain information. Otherwise it will be 0.
    OCRCha                      : SmallInt; // the OCR's character number.
    Factor                      : Single;   // A value in the range 0 to 1 giving an indication of how "good" a match was found to the character. The larger the value the better the match is.
  End; // TTOCRResultsItemExAlt 
  PTOCRResultsItemEx=^TTOCRResultsItemEx;
  TTOCRResultsItemEx= Record
    StructId                    : SmallInt; // This should be 0.
    OCRCha                      : SmallInt; // The OCR'd character number.
    Confidence                  : Single;   // Confidence in the accuracy of the character (for non-space characters only).
    XPos                        : SmallInt; // The horizontal position of the character in pixels.
    YPos                        : SmallInt; // The vertical position of the character in pixels.
    XDim                        : SmallInt; // The width of the character in pixels.
    YDim                        : SmallInt; // The height of the character in pixels.
    Alt : Array[0..4] of TTOCRResultsItemExAlt; // Up to 4 alternative chracters plus the returned OCRChar above.
  End; // TTOCRResultsItemEx 
  PTOCRResultsEx=^TTOCRResultsEx;
  TTOCRResultsEx= Record
    Header                      : TTOCRResultsHeader;        // See description for TTOCRResultsHeader
    Items                       : Array Of TTOCRResultsItemEx; // See description for TTOCRResultsItemEx
  End; // TTOCRResultsEx 

Type
  P_ByteArray=^T_ByteArray;
  T_ByteArray= Array Of Byte;

{————— Function declarations —————————————————————————————————————————————————————————————————————————————————————————————————}
Function TOCRInitialise       (var JobNo: LongInt): LongInt;                                                                         StdCall;
Function TOCRShutdown         (JobNo: LongInt): LongInt;                                                                             StdCall;
Function TOCRSetErrorMode     (JobNo: LongInt; ErrorMode: LongInt): LongInt;                                                         StdCall;
Function TOCRGetErrorMode     (JobNo: LongInt; var ErrorMode: LongInt): LongInt;                                                     StdCall;
Function TOCRGetConfig        (var JobNo: LongInt;var Parameter: LongInt;var Value:LongInt):LongInt;
Function TOCRSetConfig        (var JobNo: LongInt;var Parameter: LongInt;var Value:LongInt):LongInt;
Function TOCRGetJobDBInfo     (var JobSlotInf: LongInt): LongInt;                                                                        StdCall;
Function TOCRGetLicenceInfo   (var NumberOfJobSlots, Volume, Time, Remaining: LongInt): LongInt;                                     StdCall;
Function TOCRGetLicenceInfoEx (JobNo: LongInt; License: PChar; var Volume: LongInt; var Time: LongInt; var Remaining: LongInt; var Features: LongInt): LongInt; StdCall;
Function TOCRDoJob            (JobNo: LongInt; var JobInfo: TTOCRJobInfo): LongInt;                                                  StdCall;
Function TOCRDoJob2           (JobNo: LongInt; var JobInfo: TTOCRJobInfo2): LongInt;                                                  StdCall;
Function TOCRConvertTIFFtoDIB (JobNo: LongInt; TiffInFilename: PChar; DIBOutFilename: PChar; PageNo: LongInt) : LongInt;             StdCall;
Function TOCRConvertFormat    (JobNo: LongInt; var InputAddr: PChar; InputFormat: LongInt; var OutputAddr: PChar; OutputFormat: LongInt; PageNo: LongInt) : LongInt; StdCall;
Function TOCRGetJobStatus     (JobNo: LongInt; var JobStatus: LongInt): LongInt;                                                     StdCall;
Function TOCRGetJobStatusEx   (JobNo: LongInt; var JobStatus: LongInt; var Progress: Single; var AutoOrientation: LongInt): LongInt; StdCall;
Function TOCRGetJobStatusMsg  (JobNo: LongInt; var Msg: PChar) : LongInt;                                                            StdCall;
Function TOCRWaitForJob       (JobNo: LongInt; var JobStatus: LongInt): LongInt;                                                     StdCall;
Function TOCRWaitForAnyJob    (var WaitAnyStatus, JobNo: LongInt): LongInt;                                                          StdCall;
Function TOCRGetJobResults    (JobNo: LongInt; var ResultsInf: LongInt; Results: Pointer): LongInt;                                  StdCall;
Function TOCRGetJobResultsEx  (JobNo: LongInt; Mode: LongInt; var ResultsInf: LongInt; Results: Pointer): LongInt;                                  StdCall;
Function TOCRGetNumPages      (JobNo: LongInt; TifFilename: PChar; JobType: LongInt; Var NumPages: LongInt): LongInt;                StdCall;
Function TOCRRotateMonoBitmap (Var hBmp: LongInt; Width: LongInt; Height: LongInt; Orientation: LongInt): LongInt;                   StdCall;

Implementation

Const TOCRDLLName='TOCRdll.dll'; // DO NOT REMOVE THE .dll, or it will only work on some operating systems !!!
// Same DLL name for both 32-bit and 64-bit
{————— External declaration implementation ———————————————————————————————————————————————————————————————————————————————————}
Function TOCRInitialise      ; External TOCRDLLName;
Function TOCRShutdown        ; External TOCRDLLName;
Function TOCRSetErrorMode    ; External TOCRDLLName;
Function TOCRGetErrorMode    ; External TOCRDLLName;
Function TOCRGetConfig       ; External TOCRDLLName;
Function TOCRSetConfig       ; External TOCRDLLName;
Function TOCRGetJobDBInfo    ; External TOCRDLLName;
Function TOCRGetLicenceInfo  ; External TOCRDLLName;
Function TOCRGetLicenceInfoEx; External TOCRDLLName;
Function TOCRDoJob           ; External TOCRDLLName;
Function TOCRDoJob2          ; External TOCRDLLName;
Function TOCRConvertTIFFtoDIB; External TOCRDLLName;
Function TOCRConvertFormat   ; External TOCRDLLName;
Function TOCRGetJobStatus    ; External TOCRDLLName;
Function TOCRGetJobStatusEx  ; External TOCRDLLName;
Function TOCRGetJobStatusMsg ; External TOCRDLLName;
Function TOCRWaitForJob      ; External TOCRDLLName;
Function TOCRWaitForAnyJob   ; External TOCRDLLName;
Function TOCRGetJobResults   ; External TOCRDLLName;
Function TOCRGetJobResultsEx ; External TOCRDLLName;
Function TOCRGetNumPages     ; External TOCRDLLName;
Function TOCRRotateMonoBitmap; External TOCRDLLName;

End.
