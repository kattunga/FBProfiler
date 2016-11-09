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

unit FBTraceEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TFBTraceEventType = (
    etUnknown,
    etAttachDatabase,
    etCloseCursor,
    etCommitTransaction,
    etDetachDatabase,
    etFreeStatement,
    etTraceInit,
    etStartTransaction,
    etRollbackTransaction,
    etPrepareStatement,
    etExecuteBLR, // BLR - Binary Language Representation
    etExecuteStatementStart,
    etExecuteStatementFinish,
    etSessionStarted,
    etSessionFinished,
    etTraceFini
    );

const
  FirstFBTraceEventType = etUnknown;
  LastFBTraceEventType = etTraceFini;
  FBTraceEventTypes: array [FirstFBTraceEventType..LastFBTraceEventType] of string =
    ('UNKNOWN',
    'ATTACH_DATABASE',
    'CLOSE_CURSOR',
    'COMMIT_TRANSACTION',
    'DETACH_DATABASE',
    'FREE_STATEMENT',
    'TRACE_INIT',
    'START_TRANSACTION',
    'ROLLBACK_TRANSACTION',
    'PREPARE_STATEMENT',
    'EXECUTE_BLR',
    'EXECUTE_STATEMENT_START',
    'EXECUTE_STATEMENT_FINISH',
    'SESSION_STARTED',
    'SESSION_FINISHED',
    'TRACE_FINI'
    );

type

  TEventTransactionInfo = class
  private
    FId: string;
    FMode: string;
  public
    property Id: string read FId;
    property Mode: string read FMode;
  end;

  TEventDatabaseInfo = class
  private
    FFileName: string;
    FMode: string;
  public
    property FileName: string read FFileName;
    property Mode: string read FMode;
  end;

  { TEventStats }

  TEventStats = class
  private
    FFetches: integer;
    FMarks: integer;
    FReads: integer;
    FRecordsFetched: integer;
    FTimeMsec: integer;
    FWrites: integer;
  public
    constructor Create;
    property Fetches: integer read FFetches;
    property Marks: integer read FMarks;
    property Reads: integer read FReads;
    property RecordsFetched: integer read FRecordsFetched;
    property TimeMsec: integer read FTimeMsec;
    property Writes: integer read FWrites;
  end;

  { TEventStatement }

  TEventStatement = class
  private
    FNo: integer;
    FParams: TStrings;
    FPlan: TStrings;
    FSQL: TStrings;
  public
    constructor Create;
    destructor Destroy; override;
    property No: integer read FNo;
    property Params: TStrings read FParams;
    property Plan: TStrings read FPlan;
    property SQL: TStrings read FSQL;
  end;

  { TFBTraceEvent }

  TFBTraceEvent = class
  private
    FApplicationName: string;
    FDatabaseInfo: TEventDatabaseInfo;
    FDateTime: TDateTime;
    FEventType: TFBTraceEventType;
    FEventTypeName: string;
    FStatement: TEventStatement;
    FHeaderLinesCount: integer;
    FLines: TStrings;
    FSessionId: integer;
    FSessionName: string;
    FStats: TEventStats;
    FTransactionInfo: TEventTransactionInfo;
  public
    constructor Create;
    destructor Destroy; override;
    class function EventDateTimeToStr(DateTime: TDateTime): string;
    class function EventTypeToName(EventType: TFBTraceEventType): string;
    class function EventTypeNameToType(EventTypeName: string): TFBTraceEventType;
    property ApplicationName: string read FApplicationName;
    property DatabaseInfo: TEventDatabaseInfo read FDatabaseInfo;
    property DateTime: TDateTime read FDateTime;
    property EventType: TFBTraceEventType read FEventType;
    property EventTypeName: string read FEventTypeName;
    property Lines: TStrings read FLines;
    property SessionId: integer read FSessionId;
    property SessionName: string read FSessionName;
    property Statement: TEventStatement read FStatement;
    property Stats: TEventStats read FStats;
    property TransactionInfo: TEventTransactionInfo read FTransactionInfo;
  end;

  { TFBTraceMessageParser }

  TFBTraceMessageParser = class
  private
    FSessionId: integer;
    FSessionName: string;
    FText: string;
    FTruncatedTextLength: integer;
    procedure TrucateTextFromBegin(const ToPosNotIncluding: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Continue(AText: string);
    function NextEvent(out TraceEvent: TFBTraceEvent): boolean;
    procedure Pop;
    procedure PopSessionFinished;
    property SessionId: integer read FSessionId;
    property SessionName: string read FSessionName;
  end;

implementation

uses
  StrUtils, DateUtils,
  RegExpr;

const
  EmptyEventTimestamp = '0000-00-00T00:00:00.0000';
  EmptyEventHeader: string = EmptyEventTimestamp + ' (empty) EMPTY_EVENT';
  RegEx_EventHeader: string = '^((\d{4})-(\d{2})-(\d{2})T(\d{2}):(\d{2}):(\d{2})\.(\d{4})){1}\s+\([^\)]*\)\s+(\w+)$';
  RegEx_HeaderDbInfo: string = '^\s+([^\(]+)\(([^\)]+)\)$';
  RegEx_HeaderTxInfo: string = '^\s+\(([^,]+),\s*([^\)]+)\)$';
  RegEx_StatementLine: string = '^Statement\s+(\d+):\s*$';


function CreateRegExpr: TRegExpr;
begin
  Result := TRegExpr.Create;
  Result.ModifierI := true;
  Result.ModifierM := true;
  Result.ModifierG := false;
  Result.ModifierS := false;
  Result.ModifierR := false;
end;

{ TEventStats }

constructor TEventStats.Create;
begin
  inherited Create;
  FFetches := 0;
  FMarks := 0;
  FRecordsFetched := -1;
  FReads := 0;
  FTimeMsec := 0;
  FWrites := 0;
end;

{ TEventStatement }

constructor TEventStatement.Create;
begin
  inherited Create;
  FNo := -1;
  FParams := TStringList.Create;
  FPlan := TStringList.Create;
  FSQL := TStringList.Create;
end;

destructor TEventStatement.Destroy;
begin
  if FParams <> nil then
    FParams.Free;
  if FPlan <> nil then
    FPlan.Free;
  if FSQL <> nil then
    FSQL.Free;
  inherited Destroy;
end;

{ TFBTraceEvent }

constructor TFBTraceEvent.Create;
begin
  inherited Create;
  FEventType := etUnknown;
  FEventTypeName := '';
  FHeaderLinesCount := 0;
  FSessionId := -1;
  FLines := TStringList.Create;
  FDatabaseInfo := TEventDatabaseInfo.Create;
  FStatement := TEventStatement.Create;
  FStats := TEventStats.Create;
  FTransactionInfo := TEventTransactionInfo.Create;
end;

destructor TFBTraceEvent.Destroy;
begin
  if FLines <> nil then
    FLines.Free;
  if FDatabaseInfo <> nil then
    FDatabaseInfo.Free;
  if FStatement <> nil then
    FStatement.Free;
  if FStats <> nil then
    FStats.Free;
  if FTransactionInfo <> nil then
    FTransactionInfo.Free;
  inherited Destroy;
end;

class function TFBTraceEvent.EventDateTimeToStr(DateTime: TDateTime): string;
begin
  Result := Format('%sT%s.%.4d',
    [FormatDateTime('yyyy-mm-dd', DateTime),
    FormatDateTime('hh:nn:ss', DateTime),
    DateUtils.MilliSecondOf(DateTime) * 10]);

end;

class function TFBTraceEvent.EventTypeToName(EventType: TFBTraceEventType): string;
begin
  Result := FBTraceEventTypes[EventType];
end;

class function TFBTraceEvent.EventTypeNameToType(EventTypeName: string
  ): TFBTraceEventType;
var
  CurrType: TFBTraceEventType;
begin
  Result := etUnknown;
  for CurrType := FirstFBTraceEventType to lastFBTraceEventType do
    if FBTraceEventTypes[CurrType] = EventTypeName then
    begin
      Result := CurrType;
      break;
    end;
end;

{ TFBTraceMessageParser }

procedure TFBTraceMessageParser.TrucateTextFromBegin(const ToPosNotIncluding: integer);
begin
  if ToPosNotIncluding <= 1 then
    Exit; // Nothing to truncate
  if Length(FText) <= ToPosNotIncluding then
  begin
    FText := '';
    Inc(FTruncatedTextLength, Length(FText));
  end
  else
  begin
    FText := Copy(FText, ToPosNotIncluding, Length(FText) - ToPosNotIncluding);
    Inc(FTruncatedTextLength, ToPosNotIncluding - 1);
  end;
end;

constructor TFBTraceMessageParser.Create;
begin
  inherited Create;
  FSessionId := -1;
  FText := '';
  FTruncatedTextLength := 0;
end;

destructor TFBTraceMessageParser.Destroy;
begin
  inherited Destroy;
end;

procedure TFBTraceMessageParser.Continue(AText: string);
var
  R: TRegExpr;
begin
  R := CreateRegExpr;
  try
    R.Expression := RegEx_EventHeader;
    if R.Exec(AText) and (R.MatchPos[0] = 1) then
      FText := FText + LineEnding + AText
    else
      FText := FText + AText;
  finally
    R.Free;
  end;
end;

function TFBTraceMessageParser.NextEvent(out TraceEvent: TFBTraceEvent): boolean;

  procedure ParseFullHeader(Event: TFBTraceEvent);
  var
    R: TRegExpr;
  begin
    if Event.Lines.Count < 2 then
      Exit;
    R := CreateRegExpr;
    try
      R.Expression := RegEx_HeaderDbInfo;
      if R.Exec(Event.Lines[1]) then
      begin
        Inc(Event.FHeaderLinesCount);
        Event.DatabaseInfo.FFileName := Trim(R.Match[1]);
        Event.DatabaseInfo.FMode := Trim(R.Match[2]);
        if (Event.Lines.Count > 2) then
        begin
          R.Expression := RegEx_HeaderTxInfo;
          if R.Exec(Event.Lines[2]) then
          begin
            Inc(Event.FHeaderLinesCount);
            Event.TransactionInfo.FId := Trim(R.Match[1]);
            Event.TransactionInfo.FMode := Trim(R.Match[2]);
          end
          else
          begin
            Inc(Event.FHeaderLinesCount);
            Event.FApplicationName := Trim(Event.Lines[2]);
            if (Event.Lines.Count > 3) and R.Exec(Event.Lines[3]) then
            begin
              Inc(Event.FHeaderLinesCount);
              Event.TransactionInfo.FId := Trim(R.Match[1]);
              Event.TransactionInfo.FMode := Trim(R.Match[2]);
            end;
          end;
        end;
      end;
    finally
      R.Free;
    end;
  end;

  function ExtractTimeAndIO(Event: TFBTraceEvent; var LineIndex: integer): boolean;
  var
    R: TRegExpr;
    HasRowsFetched: boolean;
    HasTimeIO: boolean;
  begin
    Result := false;
    HasRowsFetched := false;
    HasTimeIO := false;
    if LineIndex >= Event.Lines.Count then
      Exit;
    R := CreateRegExpr;
    try
      // Are records fetched ?
      R.Expression := '(\d+)\s+records\s+fetched';
      if R.Exec(Event.Lines[LineIndex]) then
      begin
        Event.Stats.FRecordsFetched := StrToInt(R.Match[1]);
        HasRowsFetched := true;
        Inc(LineIndex);
      end;
      // Time stats and IO
      if LineIndex < Event.Lines.Count then
      begin
        R.Expression := '(\d+)\s+ms';
        if R.Exec(Event.Lines[LineIndex]) then
        begin
          Event.Stats.FTimeMsec := StrToInt(R.Match[1]);
          HasTimeIO := true;
        end;
        R.Expression := '(\d+)\s+fetch';
        if R.Exec(Event.Lines[LineIndex]) then
        begin
          Event.Stats.FFetches := StrToInt(R.Match[1]);
          HasTimeIO := true;
        end;
        R.Expression := '(\d+)\s+read';
        if R.Exec(Event.Lines[LineIndex]) then
        begin
          Event.Stats.FReads := StrToInt(R.Match[1]);
          HasTimeIO := true;
        end;
        R.Expression := '(\d+)\s+write';
        if R.Exec(Event.Lines[LineIndex]) then
        begin
          Event.Stats.FWrites := StrToInt(R.Match[1]);
          HasTimeIO := true;
        end;
        R.Expression := '(\d+)\s+mark';
        if R.Exec(Event.Lines[LineIndex]) then
        begin
          Event.Stats.FMarks := StrToInt(R.Match[1]);
          HasTimeIO := true;
        end;
        if HasTimeIO then
          Inc(LineIndex);
      end;
      Result := HasRowsFetched or HasTimeIO;
    finally
      R.Free;
    end;
  end;

  procedure ExtractPlan(Event: TFBTraceEvent; var LineIndex: integer);
  begin
    if LineIndex >= Event.Lines.Count then
      Exit;
    if SameText(Copy(Event.Lines[LineIndex], 1, 5), 'PLAN ') then
    begin
      repeat
        if SameText(Copy(Event.Lines[LineIndex], 1, 5), 'PARAM') then
          Event.Statement.Params.Append(Event.Lines[LineIndex])
        else if Trim(Event.Lines[LineIndex]) <> '' then
          Event.Statement.Plan.Append(Event.Lines[LineIndex]);
        Inc(LineIndex);
      until (LineIndex >= Event.Lines.Count) or ExtractTimeAndIO(Event, LineIndex);
    end;
  end;

  procedure ExtractStatement(Event: TFBTraceEvent; const FromLine: integer);
  var
    i: integer;
    R: TRegExpr;
    InGetText: boolean;
  begin
    InGetText := false;
    R := CreateRegExpr;
    try
      i := FromLine;
      while i < Event.Lines.Count do
      begin
        if Event.Statement.No = -1 then
        begin
          R.Expression := RegEx_StatementLine;
          if R.Exec(Event.Lines[i]) then
          begin
            Event.Statement.FNo := StrToInt(R.Match[1]);
            Inc(i);
            if i < Event.Lines.Count then
            begin
              InGetText := (Length(Event.Lines[i]) > 10) and
                (Copy(Event.Lines[i], 1, 10) = DupeString('-', 10));
              if not InGetText then
              begin
                ExtractTimeAndIO(Event, i);
                break;
              end;
            end;
          end;
        end
        else if InGetText then
        begin
          if (Length(Event.Lines[i]) > 10) and
            (Copy(Event.Lines[i], 1, 10) = DupeString('^', 10)) then
          begin
            InGetText := false;
            Inc(i);
            ExtractPlan(Event, i);
            break;
          end
          else
            Event.Statement.SQL.Add(Event.Lines[i]);
        end;
        Inc(i);
      end;
    finally
      R.Free;
    end;
  end;

  function ParseSessionStarted(Event: TFBTraceEvent): boolean;
  var
    R: TRegExpr;
  begin
    Result := false;
    R := CreateRegExpr;
    try
      R.Expression := '^Trace\s*session\s*ID\s*(\d+)\s*started$';
      if R.Exec(FText) then
      begin
        FSessionId := StrToInt(R.Match[1]);
        Event.FDateTime := Now;
        Event.FEventType := etSessionStarted;
        Event.FEventTypeName := TFBTraceEvent.EventTypeToName(Event.FEventType);
        Event.FSessionId := FSessionId;
        TrucateTextFromBegin(R.MatchPos[0] + R.MatchLen[0]);
        Result := true;
      end;
    finally
      R.Free;
    end;
  end;

  procedure ParseTraceInit(Event: TFBTraceEvent);
  var
    R: TRegExpr;
  begin
    if Event.Lines.Count < 2 then
      Exit;
    R := CreateRegExpr;
    try
      R.Expression := '^\s+SESSION\_(\d+)\s+(\S+)$';
      if R.Exec(FText) then
      begin
        Event.FSessionId := StrToInt(R.Match[1]);
        Event.FSessionName := R.Match[2];
        FSessionId := Event.FSessionId;
        if FSessionName <> Event.FSessionName then
          FSessionName := Event.FSessionName;
      end;
    finally
      R.Free;
    end;
  end;

var
  StartPos: integer;
  R: TRegExpr;
begin
  Result := false;
  TraceEvent := TFBTraceEvent.Create;
  try
    TraceEvent.FSessionId := FSessionId;
    TraceEvent.FSessionName := FSessionName;
    if FSessionId = -1 then
    begin
      Result := ParseSessionStarted(TraceEvent);
    end;
    if not Result then
    begin
      R := CreateRegExpr;
      try
        R.Expression := RegEx_EventHeader;
        if R.Exec(FText) then
        begin
          TraceEvent.FHeaderLinesCount := 1;
          TraceEvent.FDateTime := DateUtils.EncodeDateTime(
            StrToInt(R.Match[2]),
            StrToInt(R.Match[3]),
            StrToInt(R.Match[4]),
            StrToInt(R.Match[5]),
            StrToInt(R.Match[6]),
            StrToInt(R.Match[7]),
            StrToInt(R.Match[8]) div 10
            );
          TraceEvent.FEventTypeName := R.Match[9];
          TraceEvent.FEventType := TFBTraceEvent.EventTypeNameToType(TraceEvent.FEventTypeName);
          // Try to find next event
          StartPos := R.MatchPos[0];
          if R.ExecNext then
          begin
            TraceEvent.Lines.Text := Copy(FText, StartPos, R.MatchPos[0] - StartPos);
            ParseFullHeader(TraceEvent);
            case TraceEvent.FEventType of
              etAttachDatabase,
              etDetachDatabase,
              etStartTransaction: ;
              etTraceFini,
              etTraceInit:
                ParseTraceInit(TraceEvent);
              etCloseCursor,
              etExecuteBLR,
              etExecuteStatementStart,
              etExecuteStatementFinish,
              etFreeStatement,
              etPrepareStatement:
                ExtractStatement(TraceEvent, TraceEvent.FHeaderLinesCount);
              etCommitTransaction,
              etRollbackTransaction:
                ExtractTimeAndIO(TraceEvent, TraceEvent.FHeaderLinesCount);
            end;
            Result := true;
            TrucateTextFromBegin(R.MatchPos[0]);
            if R.Match[1] = EmptyEventTimestamp then
              FText := '';
          end;
        end;
      finally
        R.Free;
      end;
    end;
  finally
    if not Result then
      FreeAndNil(TraceEvent);
  end;
end;

procedure TFBTraceMessageParser.Pop;
var
  R: TRegExpr;
begin
  R := CreateRegExpr;
  try
    R.Expression := RegEx_EventHeader;
    if R.Exec(FText) then
      Continue(LineEnding + EmptyEventHeader);
  finally
    R.Free;
  end;
end;

procedure TFBTraceMessageParser.PopSessionFinished;
begin
  Continue(LineEnding + Format('%s (Session %d) %s',
    [TFBTraceEvent.EventDateTimeToStr(Now),
    SessionId,
    TFBTraceEvent.EventTypeToName(etSessionFinished)]));
  Pop;
end;

end.
