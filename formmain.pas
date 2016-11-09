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

unit FormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, StdActns, ComCtrls, ExtCtrls, DBGrids, DBCtrls,
  FBTraceEvents, XMLPropStorage,
  TraceControl;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    ActionFindNext: TAction;
    ActionFind: TAction;
    ActionClearTrace: TAction;
    ActionLoadTrace: TAction;
    ActionSaveTrace: TAction;
    ActionHelpAbout: TAction;
    ActionStopTrace: TAction;
    ActionStartTrace: TAction;
    ActionNewTrace: TAction;
    ActionListMain: TActionList;
    ActionFileExit: TFileExit;
    ActionOpenTraceConf: TFileOpen;
    DbgTraces: TDBGrid;
    DBMemoPlan: TDBMemo;
    DBMemoRaw: TDBMemo;
    DBMemoSQL: TDBMemo;
    ImageListMain: TImageList;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuMain: TMainMenu;
    DlgLoadTrace: TOpenDialog;
    TabsDetails: TPageControl;
    PanelDetails: TPanel;
    PanelList: TPanel;
    SplitterMain: TSplitter;
    StatusBarMain: TStatusBar;
    TabRaw: TTabSheet;
    TabPlan: TTabSheet;
    TabSQL: TTabSheet;
    ToolBarMain: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    PropStorage: TXMLPropStorage;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    procedure ActionClearTraceExecute(Sender: TObject);
    procedure ActionFindExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionLoadTraceExecute(Sender: TObject);
    procedure ActionNewTraceExecute(Sender: TObject);
    procedure ActionHelpAboutExecute(Sender: TObject);
    procedure ActionListMainUpdate(AAction: TBasicAction; var Handled: boolean);
    procedure ActionOpenTraceConfAccept(Sender: TObject);
    procedure ActionSaveTraceExecute(Sender: TObject);
    procedure ActionStartTraceExecute(Sender: TObject);
    procedure ActionStopTraceExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FNewTraceCounter: integer;
    procedure CheckFindTraceResult(Found: boolean);
    procedure ExceptionHanler(Sender: TObject; E: Exception);
    procedure TraceErrorHandler(Sender: TObject; const E: Exception);
    procedure ShowTraceConfDialog(Config: TTraceConfig);
  public
    procedure ShowStatus;
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  AppServices, DMMain, FormAbout, FormSearchTrace, FormEditTrace, FormSaveTrace,
  TraceFiles;

{$R *.lfm}

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);

  procedure SetDBMemoFonts(DBMemo: TDBMemo);
  begin
    DBMemo.Font.Name := 'Monospace';
    DBMemo.Font.Size := Round(Self.Canvas.TextHeight('Fj') * 0.6);
    DBMemo.Font.Pitch := fpFixed;
  end;

begin
  FNewTraceCounter := 1;
  Application.OnException := @ExceptionHanler;
  ActionOpenTraceConf.Dialog.Filter := TTraceConfFile.GetDlgFilter;
  DbgTraces.DataSource := DataModuleMain.SrcCurrTrace;
  SetDBMemoFonts(DBMemoSQL);
  SetDBMemoFonts(DBMemoPlan);
  SetDBMemoFonts(DBMemoRaw);
  DBMemoSQL.DataSource := DataModuleMain.SrcCurrTrace;
  DBMemoSQL.DataField := 'SQL';
  DBMemoPlan.DataSource := DataModuleMain.SrcCurrTrace;
  DBMemoPlan.DataField := 'Plan';
  DBMemoRaw.DataSource := DataModuleMain.SrcCurrTrace;
  DBMemoRaw.DataField := 'RawSource';
  TabsDetails.ActivePage := TabSQL;
  ShowStatus;
  GetTraces.OnTraceError := @TraceErrorHandler;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
end;

procedure TFrmMain.CheckFindTraceResult(Found: boolean);
begin
  if not Found then
    MessageDlg('No traces found', mtInformation, [mbClose], 0);
end;

procedure TFrmMain.ShowTraceConfDialog(Config: TTraceConfig);
var
  Form: TFrmEditTrace;
begin
  Form := TFrmEditTrace.Create(Application);
  try
    Config.MakeTraceName(FNewTraceCounter);
    Form.Config := Config;
    if Form.ShowModal = mrOk then
    begin
      TTraceConfFile.Save(ExpandFileName(PropStorage.FileName), Config);
      Inc(FNewTraceCounter);
      DataModuleMain.CreateTraceTable(Config.TraceName);
      GetTraces.NewTrace(Config, true);
    end
    else
      Config.Free;
  finally
    Form.Release;
    ShowStatus;
  end;
end;

procedure TFrmMain.ExceptionHanler(Sender: TObject; E: Exception);
begin
  MessageDlg(E.Message, mtError, [mbClose], 0);
end;

procedure TFrmMain.TraceErrorHandler(Sender: TObject; const E: Exception);
begin
  MessageDlg(E.Message, mtError, [mbClose], 0);
end;

procedure TFrmMain.ShowStatus;
begin
  if (GetTraces.CurrentTrace <> nil) and
    (GetTraces.CurrentTrace.Active) then
  begin
    StatusBarMain.Panels[1].Text := Format('Connected to %s:%d as %s',
      [GetTraces.CurrentTrace.Config.HostName,
      GetTraces.CurrentTrace.Config.Port,
      GetTraces.CurrentTrace.Config.UserName]);
  end
  else
  begin
    StatusBarMain.Panels[1].Text := 'Disconnected';
  end;
end;

procedure TFrmMain.ActionNewTraceExecute(Sender: TObject);
var
  Config: TTraceConfig;
begin
  Config := TTraceConfig.Create;
  try
    TTraceConfFile.Load(ExpandFileName(PropStorage.FileName), Config);
    ShowTraceConfDialog(Config);
  except
    Config.Free;
    raise;
  end;
end;

procedure TFrmMain.ActionHelpAboutExecute(Sender: TObject);
begin
  FrmAbout := TFrmAbout.Create(Application);
  try
    FrmAbout.ShowModal;
  finally
    FrmAbout.Release;
  end;
end;

procedure TFrmMain.ActionListMainUpdate(AAction: TBasicAction;
  var Handled: boolean);
begin
  ActionNewTrace.Enabled := not GetTraces.IsActiveCurrent;
  ActionOpenTraceConf.Enabled := not GetTraces.IsActiveCurrent;
  ActionSaveTrace.Enabled := not GetTraces.IsActiveCurrent and DataModuleMain.HasTrace;
  ActionLoadTrace.Enabled := not GetTraces.IsActiveCurrent;

  ActionStartTrace.Enabled := (GetTraces.CurrentTrace <> nil) and not GetTraces.IsActiveCurrent;
  ActionStopTrace.Enabled := GetTraces.IsActiveCurrent;
  ActionClearTrace.Enabled := ActionStartTrace.Enabled and ActionSaveTrace.Enabled;

  ActionFind.Enabled := ActionSaveTrace.Enabled;
  ActionFindNext.Enabled := ActionFind.Enabled and (DataModuleMain.CurrTraceFilter <> nil);

  Handled := true;
end;

procedure TFrmMain.ActionOpenTraceConfAccept(Sender: TObject);
var
  Config: TTraceConfig;
begin
  Config := TTraceConfig.Create;
  try
    TTraceConfFile.Load(ActionOpenTraceConf.Dialog.FileName, Config);
    ShowTraceConfDialog(Config);
  except
    Config.Free;
    raise;
  end;
end;

procedure TFrmMain.ActionLoadTraceExecute(Sender: TObject);
begin
  ActionStopTrace.Execute;
  DlgLoadTrace.InitialDir := GetCurrentDir();
  DlgLoadTrace.Filter := TTraceFile.GetDlgFilter;
  if DlgLoadTrace.Execute then
    DataModuleMain.LoadTrace(DlgLoadTrace.FileName);
end;

procedure TFrmMain.ActionClearTraceExecute(Sender: TObject);
begin
  if MessageDlg('Clear trace?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    DataModuleMain.ClearTrace;
    if GetTraces.CurrentTrace <> nil then
      GetTraces.CurrentTrace.ClearLog;
  end;
end;

procedure TFrmMain.ActionFindExecute(Sender: TObject);
var
  FrmSearch: TFrmSearchTrace;
begin
  FrmSearch := TFrmSearchTrace.Create(Application);
  try
    if FrmSearch.ShowModal = mrOk then
      CheckFindTraceResult(DataModuleMain.Find(sfpFirst));
  finally
    FrmSearch.Release;
  end;

end;

procedure TFrmMain.ActionFindNextExecute(Sender: TObject);
begin
  CheckFindTraceResult(DataModuleMain.Find(sfpNext));
end;

procedure TFrmMain.ActionSaveTraceExecute(Sender: TObject);
var
  FrmSaveTrace: TFrmSaveTrace;
begin
  ActionStopTrace.Execute;
  FrmSaveTrace := TFrmSaveTrace.Create(Application);
  try
    FrmSaveTrace.ShowModal;
  finally
    FrmSaveTrace.Free;
  end;
end;

procedure TFrmMain.ActionStartTraceExecute(Sender: TObject);
begin
  GetTraces.StartCurrent;
  ShowStatus;
end;

procedure TFrmMain.ActionStopTraceExecute(Sender: TObject);
begin
  GetTraces.StopCurrent;
  ShowStatus;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := not GetTraces.IsActiveCurrent;
  if GetTraces.IsActiveCurrent then
  begin
    if MessageDlg(
      'Trace is active. Stop it and exit?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      GetTraces.StopCurrent;
      CanClose := true;
    end;
  end;
end;

end.











