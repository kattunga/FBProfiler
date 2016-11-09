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

unit TraceControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL, EventLog,
  FBServices, FBTraceEvents;

type

  TTraceConfigParamType = (ptString, ptBoolean, ptInteger, ptList);

  { TTraceConfigParam }

  TTraceConfigParam = class
  private
    FName: string;
    FParamType: TTraceConfigParamType;
    FValue: string;
    FPickList: TStrings;
    procedure SetValue(AValue: string);
  public
    constructor Create(const AName, AValue: string; const AType: TTraceConfigParamType);
    destructor Destroy; override;
    property Name: string read FName;
    property Value: string read FValue write SetValue;
    property ParamType: TTraceConfigParamType read FParamType;
    property PickList: TStrings read FPickList;
  end;

  TTraceConfigParams = specialize TFPGList<TTraceConfigParam>;

  { TTraceConfig }

  TTraceConfig = class
  private
    FDatabaseFilter: string;
    FHostName: string;
    FLibPath: string;
    FParams: TTraceConfigParams;
    FPassword: string;
    FPort: integer;
    FTraceName: string;
    FUserName: string;
    FVersion: string;
    procedure SetHostName(AValue: string);
    procedure SetPassword(AValue: string);
    procedure SetPort(AValue: integer);
    procedure SetUserName(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeTraceName(Counter: integer);
    property DatabaseFilter: string read FDatabaseFilter write FDatabaseFilter;
    property HostName: string read FHostName write SetHostName;
    property LibPath: string read FLibPath write FLibPath;
    property Params: TTraceConfigParams read FParams;
    property Password: string read FPassword write SetPassword;
    property Port: integer read FPort write SetPort;
    property TraceName: string read FTraceName write FTraceName;
    property UserName: string read FUserName write SetUserName;
    property Version: string read FVersion write FVersion;
  end;

  { TTraceSession }

  TTraceSession = class
  private
    FConfig: TTraceConfig;
    FLog: TEventLog;
    FTraceService: TFBTraceService;
    FOnTraceEvent: TNotifyFBTraceEvent;
    FOnTraceMessage: TNotifyFBTraceMessageEvent;
    FOnTraceError: TNotifyFBTraceErrorEvent;
    function GetActive: boolean;
    function MakeLogFileName: string;
    procedure TraceEventHandler(const TraceEvent: TFBTraceEvent);
    procedure TraceErrorHandler(Sender: TObject; const E: Exception);
    procedure TraceMessageHandler(const TraceMessage: string);
    procedure TraceTerminateHandler(Sender: TObject);
  public
    constructor Create(AConfig: TTraceConfig);
    destructor Destroy; override;
    procedure ClearLog;
    procedure Start;
    procedure Stop;
    property Active: boolean read GetActive;
    property Config: TTraceConfig read FConfig;
    property OnTraceEvent: TNotifyFBTraceEvent
      read FOnTraceEvent write FOnTraceEvent;
    property OnTraceError: TNotifyFBTraceErrorEvent
      read FOnTraceError write FOnTraceError;
    property OnTraceMessage: TNotifyFBTraceMessageEvent
      read FOnTraceMessage write FOnTraceMessage;
  end;

  { TTraceSessionManager }

  TTraceSessionManager = class
  private
    FCurrentTrace: TTraceSession;
    FOnTraceEvent: TNotifyFBTraceEvent;
    FOnTraceMessage: TNotifyFBTraceMessageEvent;
    FOnTraceError: TNotifyFBTraceErrorEvent;
  public
    procedure NewTrace(ATraceConfig: TTraceConfig; StartIt: boolean);
    function IsActiveCurrent: boolean;
    procedure StartCurrent;
    procedure StopCurrent;
    property CurrentTrace: TTraceSession read FCurrentTrace;
    property OnTraceEvent: TNotifyFBTraceEvent
      read FOnTraceEvent write FOnTraceEvent;
    property OnTraceError: TNotifyFBTraceErrorEvent
      read FOnTraceError write FOnTraceError;
    property OnTraceMessage: TNotifyFBTraceMessageEvent
      read FOnTraceMessage write FOnTraceMessage;
  end;

implementation

uses
  StrUtils,
  AppServices,
  IBIntf;

{ TTraceConfigParam }

procedure TTraceConfigParam.SetValue(AValue: string);
begin
  if FValue = AValue then
    Exit;
  FValue := AValue;
end;

constructor TTraceConfigParam.Create(const AName, AValue: string;
  const AType: TTraceConfigParamType);
begin
  inherited Create;
  FName := AName;
  FValue := AValue;
  FParamType := AType;
  FPickList := TStringList.Create;
  if FParamType = ptBoolean then
  begin
    FPickList.Add('true');
    FPickList.Add('false');
  end;
end;

destructor TTraceConfigParam.Destroy;
begin
  FPickList.Free;
  inherited Destroy;
end;

{ TTraceConfig }

constructor TTraceConfig.Create;
begin
  inherited Create;
  FHostName := 'localhost';
  FPort := FBServices.DefaultPort;
  FLibPath := '';
  FUserName := 'SYSDBA';
  FPassword := 'masterkey';
  FDatabaseFilter := '';
  FVersion := '3';
  FParams := TTraceConfigParams.Create;
//  FParams.Add(TTraceConfigParam.Create('include_filter', '', ptString));
//  FParams.Add(TTraceConfigParam.Create('exclude_filter', '', ptString));
  FParams.Add(TTraceConfigParam.Create('log_connections', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('connection_id', '0', ptInteger));
  FParams.Add(TTraceConfigParam.Create('log_transactions', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_statement_prepare', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_statement_free', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_statement_start', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_statement_finish', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_procedure_start', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_procedure_finish', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_function_start', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_function_finish', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_trigger_start', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_trigger_finish', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_context', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_errors', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_sweep', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('print_plan', 'true', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('explain_plan', 'true', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('print_perf', 'true', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_blr_requests', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('print_blr', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('log_dyn_requests', 'true', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('print_dyn', 'false', ptBoolean));
  FParams.Add(TTraceConfigParam.Create('time_threshold', '0', ptInteger));
  {  <database>
    enabled=true
  log_transactions=false
  log_connections=true
  log_statement_prepare=false
  log_statement_free=false
  log_statement_start=true
  log_statement_finish=true
  log_blr_requests=true
  print_blr=true
  print_perf=true
  print_plan=true
  time_threshold=0
  </database>
  }
end;

destructor TTraceConfig.Destroy;
begin
  FParams.Free;
  inherited Destroy;
end;

procedure TTraceConfig.MakeTraceName(Counter: integer);
begin
  FTraceName := Format('new_trace_%d', [Counter]);
end;

procedure TTraceConfig.SetHostName(AValue: string);
begin
  if FHostName=AValue then Exit;
  FHostName:=AValue;
end;

procedure TTraceConfig.SetPassword(AValue: string);
begin
  if FPassword=AValue then Exit;
  FPassword:=AValue;
end;

procedure TTraceConfig.SetPort(AValue: integer);
begin
  if FPort=AValue then Exit;
  FPort:=AValue;
end;

procedure TTraceConfig.SetUserName(AValue: string);
begin
  if FUserName=AValue then Exit;
  FUserName:=AValue;
end;

{ TTraceSession }

constructor TTraceSession.Create(AConfig: TTraceConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FLog := TEventLog.Create(nil);
  FLog.LogType := ltFile;
end;

destructor TTraceSession.Destroy;
begin
  Stop;
  FTraceService := nil;
  FreeAndNil(FLog);
  inherited Destroy;
end;

procedure TTraceSession.ClearLog;
begin
  FLog.Active := false;
  try
    SysUtils.DeleteFile(FLog.FileName);
  finally
    FLog.Active := true;
  end;
end;

function TTraceSession.GetActive: boolean;
begin
  Result := (FTraceService <> nil);
end;

function TTraceSession.MakeLogFileName: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0))) +
    FConfig.TraceName + '.log';
end;

procedure TTraceSession.TraceEventHandler(const TraceEvent: TFBTraceEvent);
begin
  if Assigned(OnTraceEvent) then
    OnTraceEvent(TraceEvent);
end;

procedure TTraceSession.TraceErrorHandler(Sender: TObject; const E: Exception);
begin
  FLog.Error(E.Message);
  if Assigned(FOnTraceError) then
    FOnTraceError(Sender, E);
end;

procedure TTraceSession.TraceMessageHandler(const TraceMessage: string);
begin
  FLog.Info(TraceMessage);
  if Assigned(OnTraceMessage) then
    OnTraceMessage(TraceMessage);
end;

procedure TTraceSession.TraceTerminateHandler(Sender: TObject);
begin
  FTraceService := nil;
end;

procedure TTraceSession.Start;
var
  i: integer;
  Path, libPath: string;
begin

  if FConfig.LibPath = '' then
    begin
      Path := ExtractFilePath(ParamStr(0));
      libPath := Path+'libfbclient.so.3.0.0';
      if FileExists(libPath) then
        IBLibaryPath := libPath;
      libPath := Path+'libfbclient.so.2';
      if FileExists(libPath) then
        IBLibaryPath := libPath;
      libPath := Path+'libfbclient.so';
      if FileExists(libPath) then
        IBLibaryPath := libPath;
    end
  else
    IBLibaryPath := FConfig.LibPath;

  if (FTraceService = nil) and not Active then
  begin
    GetLog.Info('Creating trace...');
    FTraceService := TFBTraceService.Create(true);
    try
      FTraceService.FreeOnTerminate := false;
      FTraceService.OnTraceEvent := @TraceEventHandler;
      FTraceService.OnExceptionOccurred := @TraceErrorHandler;
      FTraceService.OnTraceMessage := @TraceMessageHandler;
      FTraceService.OnTerminate := @TraceTerminateHandler;
      // Config
      FTraceService.TraceName := FConfig.TraceName;
      GetLog.Info('Trace name: %s', [FConfig.TraceName]);
      FTraceService.ServerName := FConfig.HostName;
      GetLog.Info('Host name: %s', [FConfig.HostName]);
      FTraceService.Port := FConfig.Port;
      GetLog.Info('Port: %d', [FConfig.Port]);
      FTraceService.UserName := FConfig.UserName;
      FTraceService.Password := FConfig.Password;
      GetLog.Info('User name: %s (%s)',
        [FConfig.UserName,
        IfThen(FConfig.Password = '', 'blank password', 'password specified')]);
      FTraceService.Protocol := TCP;
      FTraceService.Configuration.Clear;

      if FConfig.Version = '3' then
        begin

          if FConfig.DatabaseFilter <> '' then
            FTraceService.Configuration.Add('database = ' + FConfig.DatabaseFilter)
          else
            FTraceService.Configuration.Add('database');
          FTraceService.Configuration.Add('{');
          FTraceService.Configuration.Add('enabled = true');
          for i := 0 to FConfig.Params.Count - 1 do
            FTraceService.Configuration.Add(
              FConfig.Params[i].Name + ' = ' + FConfig.Params[i].Value);
          FTraceService.Configuration.Add(Format('max_sql_length = %d', [FBServices.DefaultBufferSize div 2]));
          FTraceService.Configuration.Add(Format('max_blr_length = %d', [FBServices.DefaultBufferSize div 2]));
          FTraceService.Configuration.Add(Format('max_dyn_length = %d', [FBServices.DefaultBufferSize div 2]));
          FTraceService.Configuration.Add('}');

        end

      else
        begin

          if FConfig.DatabaseFilter <> '' then
            FTraceService.Configuration.Add('<database ' + FConfig.DatabaseFilter + '>')
          else
            FTraceService.Configuration.Add('<database>');
          FTraceService.Configuration.Add('enabled true');
          for i := 0 to FConfig.Params.Count - 1 do
            FTraceService.Configuration.Add(
              FConfig.Params[i].Name + ' ' + FConfig.Params[i].Value);
          FTraceService.Configuration.Add(Format('max_sql_length %d', [FBServices.DefaultBufferSize div 2]));
          FTraceService.Configuration.Add(Format('max_blr_length %d', [FBServices.DefaultBufferSize div 2]));
          FTraceService.Configuration.Add(Format('max_dyn_length %d', [FBServices.DefaultBufferSize div 2]));
          FTraceService.Configuration.Add('</database>');

        end;

      GetLog.Info('Configuration:' + LineEnding + FTraceService.Configuration.Text);

      GetLog.Info('Starting trace %s', [FConfig.TraceName]);
      FLog.Active := false;
      FLog.AppendContent := (FLog.FileName = MakeLogFileName);
      FLog.FileName := MakeLogFileName;
      FLog.Active := true;
      GetLog.Info('Trace log redirected to file: %s', [MakeLogFileName]);
    except
      on E: Exception do
      begin
        if Assigned(FOnTraceError) then
          FOnTraceError(FTraceService, E);
        FreeAndNil(FTraceService);
      end;
    end;
    FTraceService.Start;
  end;
end;

procedure TTraceSession.Stop;
begin
  if FTraceService <> nil then
  begin
    FTraceService.Terminate;
    FTraceService.WaitFor;
    FreeAndNil(FTraceService);
  end;
end;

{ TTraceSessionManager }

procedure TTraceSessionManager.NewTrace(ATraceConfig: TTraceConfig;
  StartIt: boolean);
begin
  if FCurrentTrace <> nil then
  begin
    FCurrentTrace.Stop;
    FreeAndNil(FCurrentTrace);
  end;
  FCurrentTrace := TTraceSession.Create(ATraceConfig);
  FCurrentTrace.OnTraceEvent := Self.OnTraceEvent;
  FCurrentTrace.OnTraceError := Self.OnTraceError;
  FCurrentTrace.OnTraceMessage := Self.OnTraceMessage;
  if StartIt and not FCurrentTrace.Active then
    FCurrentTrace.Start;
end;

function TTraceSessionManager.IsActiveCurrent: boolean;
begin
  Result := (FCurrentTrace <> nil) and FCurrentTrace.Active;
end;

procedure TTraceSessionManager.StartCurrent;
begin
  if FCurrentTrace <> nil then
    FCurrentTrace.Start;
end;

procedure TTraceSessionManager.StopCurrent;
begin
  if FCurrentTrace <> nil then
  begin
    FCurrentTrace.Stop;
  end;
end;

end.

