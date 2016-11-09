unit FBTraceEventsTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  FBTraceEvents;

type

  { TFBTraceMessageParserTest }

  TFBTraceMessageParserTest = class(TTestCase)
  protected
    //procedure SetUp; override;
    //procedure TearDown; override;
    procedure ParseEvent(const FileName: string; var Event: TFBTraceEvent);
  published
    procedure TestEventSequence;
    procedure TestParseAttachDatabase;
    procedure TestParseCloseCursor;
    procedure TestParseCommitTransaction;
    procedure TestParseFreeStatement;
    procedure TestParseExecuteBLR;
    procedure TestParseExecuteStatementStart;
    procedure TestParseExecuteStatementFinish;
    procedure TestParseSessionStarted;
    procedure TestParseTraceInit;
    procedure TestParseStartTransaction;
    procedure TestParseRollbackTransaction;
    procedure TestParsePrepareStatement;
  end;

function LoadTrace(const FileName: string): string;
function MakeTestFileName(const FileName: string): string;

implementation

uses
  DateUtils;

function LoadTrace(const FileName: string): string;
var
  FS: TFileStream;
begin
  TAssert.AssertTrue(Format('File not exists: %s', [MakeTestFileName(FileName)]),
    FileExists(MakeTestFileName(FileName)));
  FS := TFileStream.Create(MakeTestFileName(FileName), fmOpenRead);
  try
    SetLength(Result, FS.Size);
    FS.Seek(0, soBeginning);
    FS.Read(Result[1], FS.Size);
  finally
    FS.Free;
  end;
end;

function MakeTestFileName(const FileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) +
    'testdata' + PathDelim + FileName;
end;

procedure TFBTraceMessageParserTest.ParseEvent(const FileName: string;
  var Event: TFBTraceEvent);
var
  NextEvent: TFBTraceEvent;
  Parser: TFBTraceMessageParser;
begin
  Parser := TFBTraceMessageParser.Create;
  try
    Parser.Continue(LoadTrace(FileName));
    AssertTrue(Format('Not parsed. File: %s', [FileName]), Parser.NextEvent(Event));
    AssertFalse(Format('Found more events.File: %s', [FileName]), Parser.NextEvent(NextEvent));
  finally
    Parser.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestEventSequence;
var
  Event: TFBTraceEvent;
  Parser: TFBTraceMessageParser;
  i, EventsCount, AttendedEventsCount: integer;
  FileName: string;
begin
  Parser := TFBTraceMessageParser.Create;
  try
    for i := 1 to 7 do
    begin
      EventsCount := 0;
      FileName := Format('trace_test_%.2d.txt', [i]);
      Parser.Continue(LoadTrace(FileName));
      if i = 7 then
        Parser.Pop;
      while Parser.NextEvent(Event) do
      begin
        Inc(EventsCount);
        Writeln(Format('%s: %d. %s', [FileName, EventsCount, Event.EventTypeName]));
        AssertFalse(Format('Unknown event type: %s. File: %s', [Event.EventTypeName, FileName]),
          Event.EventType = etUnknown);
      end;
{ $ grep -c "2013-06-20T" trace_test_*
trace_test_01.txt:55
trace_test_02.txt:8
trace_test_03.txt:38
trace_test_04.txt:8
trace_test_05.txt:2
trace_test_06.txt:9
trace_test_07.txt:4
}
      case i of
        1: AttendedEventsCount := 54;
        2: AttendedEventsCount := 7 + 1;
        3: AttendedEventsCount := 37 + 1;
        4: AttendedEventsCount := 7 + 1;
        5: AttendedEventsCount := 1 + 1;
        6: AttendedEventsCount := 8 + 1;
        7: AttendedEventsCount := 4 + 1;
      end;
      AssertEquals(Format('Not all events was parsed. File: %s', [FileName]),
        AttendedEventsCount, EventsCount);
    end;
    Parser.Pop;
    AssertFalse('Pop on empty trace', Parser.NextEvent(Event));
    Parser.PopSessionFinished;
    AssertTrue('Pop session finished', Parser.NextEvent(Event));
    AssertEquals(Integer(etSessionFinished), Integer(Event.EventType));
  finally
    Parser.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseAttachDatabase;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_attach_database.txt', Event);
  try
    AssertTrue(etAttachDatabase = Event.EventType);
    AssertEquals('D:\BELAIR3\APPTEST\APPTEST.GDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_16, SYSDBA:NONE, ISO88591, TCPv4:127.0.0.1', Event.DatabaseInfo.Mode);
    AssertEquals('C:\Program Files\FlameRobin\flamerobin.exe:3784', Event.ApplicationName);
    AssertEquals(-1, Event.Statement.No);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseCommitTransaction;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_commit_transaction.txt', Event);
  try
    AssertTrue(etCommitTransaction = Event.EventType);
    AssertEquals('D:\BELAIR3\APPTEST\APPTEST.GDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_16, SYSDBA:NONE, ISO88591, TCPv4:127.0.0.1', Event.DatabaseInfo.Mode);
    AssertEquals('C:\Program Files\FlameRobin\flamerobin.exe:3784', Event.ApplicationName);
    AssertEquals('TRA_37', Event.TransactionInfo.Id);
    AssertEquals('CONCURRENCY | WAIT | READ_WRITE', Event.TransactionInfo.Mode);
    AssertEquals(-1, Event.Statement.No);
    AssertEquals(34826, Event.Stats.TimeMsec);
    AssertEquals(462378, Event.Stats.Writes);
    AssertEquals(78954, Event.Stats.Fetches);
    AssertEquals(3, Event.Stats.Marks);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseFreeStatement;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_free_statement.txt', Event);
  try
    AssertTrue(etFreeStatement = Event.EventType);
    AssertEquals('D:\BELAIR3\APPTEST\APPTEST.GDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_16, SYSDBA:NONE, ISO88591, TCPv4:127.0.0.1', Event.DatabaseInfo.Mode);
    AssertEquals('', Event.TransactionInfo.Id);
    AssertEquals(50, Event.Statement.No);
    AssertEquals(1, Event.Statement.SQL.Count);
    AssertEquals('select count(*) from rdb$fields f left outer join rdb$types t on f.rdb$field_type=t.rdb$type where t.rdb$field_name=''RDB$FIELD_TYPE'' and f.rdb$field_name = ?',
      Event.Statement.SQL[0]);
    AssertEquals(1, Event.Statement.Plan.Count);
    AssertEquals('PLAN JOIN (F INDEX (RDB$INDEX_2), T NATURAL)',
      Event.Statement.Plan[0]);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseSessionStarted;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_session_started.txt', Event);
  try
    AssertTrue(etSessionStarted = Event.EventType);
    AssertEquals(35, Event.SessionId);
    AssertEquals(TFBTraceEvent.EventTypeToName(etSessionStarted), Event.EventTypeName);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseTraceInit;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_trace_init.txt', Event);
  try
    AssertTrue(etTraceInit = Event.EventType);
    AssertEquals(35, Event.SessionId);
    AssertEquals('new_trace_1', Event.SessionName);
    AssertEquals(2013, DateUtils.YearOf(Event.DateTime));
    AssertEquals(06, DateUtils.MonthOf(Event.DateTime));
    AssertEquals(20, DateUtils.DayOf(Event.DateTime));
    AssertEquals(13, DateUtils.HourOf(Event.DateTime));
    AssertEquals(55, DateUtils.MinuteOf(Event.DateTime));
    AssertEquals(41, DateUtils.SecondOf(Event.DateTime));
    AssertEquals(66, DateUtils.MilliSecondOf(Event.DateTime));
    AssertEquals(-1, Event.Statement.No);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseStartTransaction;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_start_transaction_01.txt', Event);
  try
    AssertTrue(etStartTransaction = Event.EventType);
    AssertEquals('C:\PROGRAM FILES\FIREBIRD\FIREBIRD_2_5\SECURITY2.FDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_489, SYSDBA:NONE, NONE, <internal>', Event.DatabaseInfo.Mode);
    AssertEquals('TRA_1446', Event.TransactionInfo.Id);
    AssertEquals('CONCURRENCY | WAIT | READ_ONLY', Event.TransactionInfo.Mode);
    AssertEquals(-1, Event.Statement.No);
    Event.Free;
    ParseEvent('u_start_transaction_02.txt', Event);
    AssertEquals('D:\BELAIR3\APPTEST\APPTEST.GDB', Event.DatabaseInfo.FileName);
    AssertEquals('TRA_35', Event.TransactionInfo.Id);
    AssertEquals('CONCURRENCY | WAIT | READ_WRITE', Event.TransactionInfo.Mode);
    AssertEquals('C:\Program Files\FlameRobin\flamerobin.exe:3784', Event.ApplicationName);
    AssertEquals(-1, Event.Statement.No);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseRollbackTransaction;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_rollback_transaction.txt', Event);
  try
    AssertTrue(etRollbackTransaction = Event.EventType);
    AssertEquals('C:\PROGRAM FILES\FIREBIRD\FIREBIRD_2_5\SECURITY2.FDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_489, SYSDBA:NONE, NONE, <internal>', Event.DatabaseInfo.Mode);
    AssertEquals('TRA_1446', Event.TransactionInfo.Id);
    AssertEquals('CONCURRENCY | WAIT | READ_ONLY', Event.TransactionInfo.Mode);
    AssertEquals(-1, Event.Statement.No);
    AssertEquals('', Event.Statement.SQL.Text);
    AssertEquals(10, Event.Stats.TimeMsec);
    AssertEquals(1, Event.Stats.Writes);
    AssertEquals(2, Event.Stats.Fetches);
    AssertEquals(3, Event.Stats.Marks);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParsePrepareStatement;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_prepare_statement.txt', Event);
  try
    AssertTrue(etPrepareStatement = Event.EventType);
    AssertEquals('D:\BELAIR3\APPTEST\APPTEST.GDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_16, SYSDBA:NONE, ISO88591, TCPv4:127.0.0.1', Event.DatabaseInfo.Mode);
    AssertEquals('TRA_35', Event.TransactionInfo.Id);
    AssertEquals('CONCURRENCY | WAIT | READ_ONLY', Event.TransactionInfo.Mode);
    AssertEquals(36, Event.Statement.No);
    AssertEquals(2, Event.Statement.SQL.Count);
    AssertEquals('select rdb$character_set_name', Event.Statement.SQL[0]);
    AssertEquals('from rdb$database', Event.Statement.SQL[1]);
    AssertEquals(22, Event.Stats.TimeMsec);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseExecuteStatementStart;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_execute_statement_start.txt', Event);
  try
    AssertTrue(etExecuteStatementStart = Event.EventType);
    AssertEquals('D:\BELAIR3\APPTEST\APPTEST.GDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_16, SYSDBA:NONE, ISO88591, TCPv4:127.0.0.1', Event.DatabaseInfo.Mode);
    AssertEquals('TRA_36', Event.TransactionInfo.Id);
    AssertEquals('CONCURRENCY | WAIT | READ_ONLY', Event.TransactionInfo.Mode);
    AssertEquals(51, Event.Statement.No);
    AssertEquals(1, Event.Statement.SQL.Count);
    AssertEquals('select f.rdb$field_type, f.rdb$field_sub_type, f.rdb$field_length, f.rdb$field_precision, f.rdb$field_scale, c.rdb$character_set_name,  f.rdb$character_length, f.rdb$null_flag, f.rdb$default_source,  l.rdb$collation_name, f.rdb$validation_source, f.rdb$computed_blr,  c.rdb$bytes_per_character  fr...',
      Event.Statement.SQL[0]);
    AssertEquals(1, Event.Statement.Plan.Count);
    AssertEquals('PLAN JOIN (JOIN (F INDEX (RDB$INDEX_2), C INDEX (RDB$INDEX_25)), L INDEX (RDB$INDEX_26))',
      Event.Statement.Plan[0]);
    AssertEquals(1, Event.Statement.Params.Count);
    AssertEquals('param0 = char(31), "RDB$4227                       "',
      Event.Statement.Params[0]);
    AssertEquals(-1, Event.Stats.RecordsFetched);
    AssertEquals(0, Event.Stats.TimeMsec);
    AssertEquals(0, Event.Stats.Reads);
    AssertEquals(0, Event.Stats.Fetches);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseExecuteStatementFinish;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_execute_statement_finish.txt', Event);
  try
    AssertTrue(etExecuteStatementFinish = Event.EventType);
    AssertEquals('D:\BELAIR3\APPTEST\APPTEST.GDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_16, SYSDBA:NONE, ISO88591, TCPv4:127.0.0.1', Event.DatabaseInfo.Mode);
    AssertEquals('TRA_36', Event.TransactionInfo.Id);
    AssertEquals('CONCURRENCY | WAIT | READ_ONLY', Event.TransactionInfo.Mode);
    AssertEquals(50, Event.Statement.No);
    AssertEquals(1, Event.Statement.SQL.Count);
    AssertEquals('select count(*) from rdb$fields f left outer join rdb$types t on f.rdb$field_type=t.rdb$type where t.rdb$field_name=''RDB$FIELD_TYPE'' and f.rdb$field_name = ?',
      Event.Statement.SQL[0]);
    AssertEquals(1, Event.Statement.Plan.Count);
    AssertEquals('PLAN JOIN (F INDEX (RDB$INDEX_2), T NATURAL)',
      Event.Statement.Plan[0]);
    AssertEquals(1, Event.Statement.Params.Count);
    AssertEquals('param0 = char(31), "RDB$4227                       "',
      Event.Statement.Params[0]);
    AssertEquals(1, Event.Stats.RecordsFetched);
    AssertEquals(12, Event.Stats.TimeMsec);
    AssertEquals(2, Event.Stats.Reads);
    AssertEquals(471, Event.Stats.Fetches);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseExecuteBLR;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_execute_blr.txt', Event);
  try
    AssertTrue(etExecuteBLR = Event.EventType);
    AssertEquals('C:\PROGRAM FILES\FIREBIRD\FIREBIRD_2_5\SECURITY2.FDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_489, SYSDBA:NONE, NONE, <internal>', Event.DatabaseInfo.Mode);
    AssertEquals('TRA_1446', Event.TransactionInfo.Id);
    AssertEquals('CONCURRENCY | WAIT | READ_ONLY', Event.TransactionInfo.Mode);
    AssertEquals(28, Event.Statement.No);
    AssertEquals('', Event.Statement.SQL.Text);
    AssertEquals(20, Event.Stats.TimeMsec);
    AssertEquals(4, Event.Stats.Fetches);
  finally
    Event.Free;
  end;
end;

procedure TFBTraceMessageParserTest.TestParseCloseCursor;
var
  Event: TFBTraceEvent = nil;
begin
  ParseEvent('u_close_cursor.txt', Event);
  try
    AssertTrue(etCloseCursor = Event.EventType);
    AssertEquals('D:\BELAIR3\APPTEST\APPTEST.GDB', Event.DatabaseInfo.FileName);
    AssertEquals('ATT_16, SYSDBA:NONE, ISO88591, TCPv4:127.0.0.1', Event.DatabaseInfo.Mode);
    AssertEquals('C:\Program Files\FlameRobin\flamerobin.exe:3784', Event.ApplicationName);
    AssertEquals(52, Event.Statement.No);
    AssertEquals(1, Event.Statement.SQL.Count);
    AssertEquals('select r.rdb$constraint_name, i.rdb$field_name, r.rdb$index_name from rdb$relation_constraints r, rdb$index_segments i where r.rdb$relation_name=? and r.rdb$index_name=i.rdb$index_name and (r.rdb$constraint_type=''PRIMARY KEY'') order by r.rdb$constraint_name, i.rdb$field_position',
      Event.Statement.SQL[0]);
    AssertEquals(1, Event.Statement.Plan.Count);
    AssertEquals('PLAN SORT (JOIN (R INDEX (RDB$INDEX_42), I INDEX (RDB$INDEX_6)))',
      Event.Statement.Plan[0]);
  finally
    Event.Free;
  end;
end;

initialization
  RegisterTest(TFBTraceMessageParserTest);

end.

