program fbprofilercmd;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Interfaces, fbservices;

type

  { TTracerApp }

  TTracerApp = class
  private
    FTraceService: TFBTraceService;
    procedure OnTraceMessage(const TraceMessage: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Run;
  end;

{ TTracerApp }

procedure TTracerApp.OnTraceMessage(const TraceMessage: string);
begin
  writeln(TraceMessage);
end;

constructor TTracerApp.Create;
begin
  inherited Create;
  FTraceService := TFBTraceService.Create(true);
end;

destructor TTracerApp.Destroy;
begin
  FTraceService.Terminate;
  inherited Destroy;
end;

procedure TTracerApp.Run;
begin
  FTraceService.ServerName := 'localhost';
  FTraceService.Configuration.Clear;
  FTraceService.Configuration.Text :=
    '<database>' + sLineBreak +
    'enabled true ' + sLineBreak +
    'log_transactions false ' + sLineBreak +
    'log_connections true ' + sLineBreak +
    'log_statement_prepare false ' + sLineBreak +
    'log_statement_free false ' + sLineBreak +
    'log_statement_start true ' + sLineBreak +
    'log_statement_finish true ' + sLineBreak +
    'log_blr_requests true ' + sLineBreak +
    'print_blr true ' + sLineBreak +
    'print_perf true ' + sLineBreak +
    'print_plan true ' + sLineBreak +
    'time_threshold 0 ' + sLineBreak +
    '</database>';
  FTraceService.UserName := 'SYSDBA';
  FTraceService.Password := 'masterkey';
  //FTraceService.Protocol := TCP;
  FTraceService.OnTraceMessage := @OnTraceMessage;
  FTraceService.Start;
  FTraceService.WaitFor;
end;

var
  TracerApp: TTracerApp;

{$R *.res}

begin
  TracerApp := TTracerApp.Create;
  try
    TracerApp.Run;
  finally
    TracerApp.Free;
  end;
end.

