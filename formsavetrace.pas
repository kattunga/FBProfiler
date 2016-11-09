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

unit FormSaveTrace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons;

type

  { TFrmSaveTrace }

  TFrmSaveTrace = class(TForm)
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    BtnSelectDir: TBitBtn;
    EdtTargetDir: TLabeledEdit;
    EdtPackage: TLabeledEdit;
    DlgTargetDir: TSelectDirectoryDialog;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnSelectDirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FrmSaveTrace: TFrmSaveTrace;

implementation

{$R *.lfm}

uses
  AppServices,
  TraceFiles,
  DMMain;

{ TFrmSaveTrace }

procedure TFrmSaveTrace.FormCreate(Sender: TObject);
begin
  EdtTargetDir.Text := ExtractFileDir(ParamStr(0));
  if GetTraces.CurrentTrace <> nil then
    EdtPackage.Text := TTraceFile.MakePackName(GetTraces.CurrentTrace.Config.TraceName)
  else
    EdtPackage.Text := TTraceFile.MakePackName(DataModuleMain.CurrTraceName);
end;

procedure TFrmSaveTrace.BtnSelectDirClick(Sender: TObject);
begin
  if DlgTargetDir.Execute then
  begin
    EdtTargetDir.Text := DlgTargetDir.FileName;
  end;
end;

procedure TFrmSaveTrace.BtnOKClick(Sender: TObject);
begin
  DataModuleMain.SaveTrace(EdtTargetDir.Text, EdtPackage.Text);
  Close;
end;

end.

