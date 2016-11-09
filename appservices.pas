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

unit AppServices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, EventLog,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}

  VersionResource, VersionTypes,
  TraceControl;

function GetLog: TEventLog;
function GetTraces: TTraceSessionManager;
function GetVersionInfo: TVersionFixedInfo;

implementation

var
  TraceManager: TTraceSessionManager = nil;
  Log: TEventLog = nil;
  FileVersionResource: TVersionResource = nil;

function GetLog: TEventLog;
begin
  if Log = nil then
  begin
    Log := TEventLog.Create(nil);
    Log.FileName := ParamStr(0) + '.log';
    Log.LogType := ltFile;
    Log.Active := True;
  end;
  Result := Log;
end;

function GetTraces: TTraceSessionManager;
begin
  if TraceManager = nil then
    TraceManager := TTraceSessionManager.Create;
  Result := TraceManager;
end;

function GetVersionInfo: TVersionFixedInfo;
var
  Stream: TResourceStream;
begin
  if FileVersionResource = nil then
  begin
    FileVersionResource := TVersionResource.Create;
    Stream := TResourceStream.CreateFromID(HINSTANCE, 1, PChar(RT_VERSION));
    try
      FileVersionResource.SetCustomRawDataStream(Stream);
      Result := FileVersionResource.FixedInfo; // load data
      FileVersionResource.SetCustomRawDataStream(nil);
    finally
      Stream.Free
    end;
  end
  else
    Result := FileVersionResource.FixedInfo;
end;

initialization

finalization
  if TraceManager <> nil then
    FreeAndNil(TraceManager);
  if Log <> nil then
    FreeAndNil(Log);
  if FileVersionResource = nil then
    FreeAndNil(FileVersionResource);
end.
