{---------------------------------------------------------------------}
{                                                                     }
{ Firebird database server profiler tool (FBProfiler)                 }
{                                                                     }
{ Copyright (c) 2013-2015 Bel Air Informatique (www.belair-info.fr)   }
{ Copyright (c) 2013-2015 Serguei Tarassov (www.arbinada.com)         }
{                                                                     }
{ FBProfiler uses IBX For Lazarus components (Firebird Express)       }
{ FBProfiler was firstly released for public domain                   }
{ on October 31 of 2015 (Halloween) so any donations are welcome.     }
{                                                                     }
{ The contents of this file are subject to the InterBase              }
{ Public License Version 1.0 (the "License"); you may not             }
{ use this file except in compliance with the License. You            }
{ may obtain a copy of the License at                                 }
{ http://www.firebirdsql.org/en/interbase-public-license/             }
{ Software distributed under the License is distributed on            }
{ an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either              }
{ express or implied. See the License for the specific language       }
{ governing rights and limitations under the License.                 }
{                                                                     }
{---------------------------------------------------------------------}

unit DMMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Dbf, Dbf_DbfFile,
  FileUtil,
  FBTraceEvents;

type
  { TTraceFilter }

  TTraceFilter = class
  public
    PlanFilter: boolean;
    PlanExpr: string;
    SQLFilter: boolean;
    SQLExpr: string;
    DurationMsFilter: boolean;
    DurationMsMin, DurationMsMax: integer;
    constructor Create;
  end;

  TSearchFromPosition = (sfpFirst, sfpNext);

  { TDataModuleMain }

  TDataModuleMain = class(TDataModule)
    SrcCurrTrace: TDatasource;
    DsCurrTrace: TDbf;
    procedure DataModuleCreate(Sender: TObject);
  private
    FEventCounter: integer;
    procedure OpenTraceFile(const FileName: string);
    procedure OnTraceEvent(const TraceEvent: TFBTraceEvent);
  public
    CurrTraceFilter: TTraceFilter;
    function CurrTraceName: string;
    function HasTrace: boolean;
    function Find(const FromPosition: TSearchFromPosition): boolean;
    procedure CreateTraceTable(const TraceName: string);
    procedure ClearTrace;
    procedure SaveTrace(const TargetDir, PackageName: string);
    procedure LoadTrace(const FileName: string);
  end;

var
  DataModuleMain: TDataModuleMain;

implementation

{$R *.lfm}

uses
  StrUtils,
  Forms,
  AppServices,
  TraceFiles;

{ TTraceFilter }

constructor TTraceFilter.Create;
begin
  inherited Create;
  PlanFilter := false;
  PlanExpr := '';
  SQLFilter := false;
  SQLExpr := '';
  DurationMsFilter := false;
  DurationMsMin := 0;
  DurationMsMax := MAXINT;
end;

{ TDataModuleMain }

procedure TDataModuleMain.DataModuleCreate(Sender: TObject);
begin
  FEventCounter := 1;
  CurrTraceFilter := nil;
  GetLog.Info('%s started', [Application.Title]);
  GetTraces.OnTraceEvent := @Self.OnTraceEvent;
  // DsCurrTrace.LanguageID := 1033;
  // DsGlobals.DefaultCreateCodePage := 1252;
  DsCurrTrace.TableLevel := 25;
end;

procedure TDataModuleMain.OpenTraceFile(const FileName: string);
begin
  Assert(SysUtils.FileExists(FileName), Format('Trace file not found: %s', [FileName]));
  DsCurrTrace.DisableControls;
  try
    DsCurrTrace.FilePathFull := ExtractFileDir(FileName);
    DsCurrTrace.TableName := ExtractFileName(FileName);
    //DsCurrTrace.Exclusive := true;
    //DsCurrTrace.Open;
    //DsCurrTrace.DbfFile.UseCodePage := 1252;
    //DsCurrTrace.AddIndex('traceid', 'Id', [ixPrimary, ixUnique]);
    //DsCurrTrace.Close;
    DsCurrTrace.Exclusive := false;
    DsCurrTrace.Open;
    if DsCurrTrace.RecordCount > 0 then
    begin
      DsCurrTrace.Last;
      FEventCounter := DsCurrTrace.FieldByName('Id').AsInteger + 1;
    end
    else
      FEventCounter := 1;
  finally
    DsCurrTrace.EnableControls;
  end;
end;

procedure TDataModuleMain.OnTraceEvent(const TraceEvent: TFBTraceEvent);
begin
  if TraceEvent.EventType in [etTraceInit, etTraceFini] then Exit;

  DsCurrTrace.Append;
  DsCurrTrace.FieldByName('Id').AsInteger := FEventCounter;
  DsCurrTrace.FieldByName('SessionId').AsInteger := TraceEvent.SessionId;
  DsCurrTrace.FieldByName('EvDate').AsDateTime := TraceEvent.DateTime;
  DsCurrTrace.FieldByName('Statement').AsInteger := TraceEvent.Statement.No;
  DsCurrTrace.FieldByName('EvTypeId').AsInteger := Ord(TraceEvent.EventType);
  DsCurrTrace.FieldByName('EvTypeName').AsString := TraceEvent.EventTypeName;
  DsCurrTrace.FieldByName('AppName').AsString := TraceEvent.ApplicationName;
  DsCurrTrace.FieldByName('RawSource').AsString := TraceEvent.Lines.Text;
  // Stats
  DsCurrTrace.FieldByName('Duration').AsInteger := TraceEvent.Stats.TimeMsec;
  DsCurrTrace.FieldByName('Reads').AsInteger := TraceEvent.Stats.Reads;
  DsCurrTrace.FieldByName('Writes').AsInteger := TraceEvent.Stats.Writes;
  DsCurrTrace.FieldByName('Fetches').AsInteger := TraceEvent.Stats.Fetches;
  DsCurrTrace.FieldByName('Marks').AsInteger := TraceEvent.Stats.Marks;
  DsCurrTrace.FieldByName('RecFetched').AsInteger := TraceEvent.Stats.RecordsFetched;
  // Statement
  DsCurrTrace.FieldByName('SQL').AsString := TraceEvent.Statement.SQL.Text;
  DsCurrTrace.FieldByName('Plan').AsString := TraceEvent.Statement.Plan.Text;
  DsCurrTrace.Post;
  Inc(FEventCounter);
end;

function TDataModuleMain.CurrTraceName: string;
begin
  Result := ChangeFileExt(DsCurrTrace.TableName, '');
end;

function TDataModuleMain.HasTrace: boolean;
begin
  Result := DsCurrTrace.Active and (DsCurrTrace.RecordCount > 0);
end;

function TDataModuleMain.Find(const FromPosition: TSearchFromPosition): boolean;
var
  CurrRec: integer;
begin
  Result := false;
  if (CurrTraceFilter = nil) or not DsCurrTrace.Active or (DsCurrTrace.RecordCount = 0) then
    Exit;
  CurrRec := DsCurrTrace.RecNo;
  DsCurrTrace.DisableControls;
  try
    case FromPosition of
      sfpFirst: DsCurrTrace.First;
      sfpNext: DsCurrTrace.Next;
    end;
    while not DsCurrTrace.EOF do
    begin
      Result := true;
      if CurrTraceFilter.DurationMsFilter then
        Result := Result and
          (DsCurrTrace.FieldByName('Duration').AsInteger >= CurrTraceFilter.DurationMsMin) and
          (DsCurrTrace.FieldByName('Duration').AsInteger <= CurrTraceFilter.DurationMsMax);
      if CurrTraceFilter.SQLFilter then
        Result := Result and
          AnsiContainsText(DsCurrTrace.FieldByName('SQL').AsString, CurrTraceFilter.SQLExpr);
      if CurrTraceFilter.PlanFilter then
        Result := Result and
          AnsiContainsText(DsCurrTrace.FieldByName('Plan').AsString, CurrTraceFilter.PlanExpr);
      if Result then
        break;
      DsCurrTrace.Next;
    end;
    if not Result then
      DsCurrTrace.RecNo := CurrRec;
  finally
    DsCurrTrace.EnableControls;
  end;
end;

procedure TDataModuleMain.CreateTraceTable(const TraceName: string);
begin
  if DsCurrTrace.Active then
    DsCurrTrace.DisableControls;
  try
    DsCurrTrace.Close;
    DsCurrTrace.FilePathFull := ExtractFileDir(ParamStr(0));
    DsCurrTrace.TableName := TraceName + '.dbf';
    with DsCurrTrace.FieldDefs do
    begin
      Clear;
      Add('Id', ftInteger, 0, true);
      Add('SessionId', ftInteger, 0, true);
      Add('Statement', ftInteger, 0, true);
      Add('EvDate', ftString, 24, true);        // bug in fpc 3.0 windows 32 bits //Add('EvDate', ftDateTime, 0, true);
      Add('EvTypeId', ftSmallint, 0, true);
      Add('EvTypeName', ftString, 24, true);
      Add('AppName', ftString, 100, true);
      // Stats
      Add('Duration', ftInteger, 0, false);
      Add('Reads', ftInteger, 0, false);
      Add('Writes', ftInteger, 0, false);
      Add('Fetches', ftInteger, 0, false);
      Add('Marks', ftInteger, 0, false);
      Add('RecFetched', ftInteger, 0, false);
      // Statement
      Add('SQL', ftMemo, 0, false);
      Add('Plan', ftMemo, 0, false);
      Add('RawSource', ftMemo, 0, false);
    end;
    DsCurrTrace.CreateTable;
    OpenTraceFile(IncludeTrailingPathDelimiter(DsCurrTrace.FilePathFull) + DsCurrTrace.TableName);
  finally
    if DsCurrTrace.Active then
      DsCurrTrace.EnableControls;
  end;
end;

procedure TDataModuleMain.ClearTrace;
begin
  if DsCurrTrace.Active then
  begin
    DsCurrTrace.DisableControls;
    try
      DsCurrTrace.TryExclusive;
      try
        DsCurrTrace.Zap;
        DsCurrTrace.PackTable;
        FEventCounter := 1;
        DsCurrTrace.Refresh;
      finally
        DsCurrTrace.EndExclusive;
      end;
    finally
      DsCurrTrace.EnableControls;
    end;
  end;
end;

procedure TDataModuleMain.SaveTrace(const TargetDir, PackageName: string);
begin
  DsCurrTrace.Active := false;
  try
    TTraceFile.Save(DsCurrTrace.FilePath, DsCurrTrace.TableName, TargetDir, PackageName);
  finally
    DsCurrTrace.Active := true;
  end;
end;

procedure TDataModuleMain.LoadTrace(const FileName: string);
var
  DbFileName: string;
begin
  DsCurrTrace.Active := false;
  try
    DbFileName := TTraceFile.Restore(FileName);
    OpenTraceFile(DbFileName);
  finally
    DsCurrTrace.Active := true;
  end;
end;

end.
