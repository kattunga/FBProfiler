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

unit FormEditTrace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ValEdit, Spin, StdCtrls,
  TraceControl;

type

  { TFrmEditTrace }

  TFrmEditTrace = class(TForm)
    BtnSaveConf: TBitBtn;
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    edtVersion: TComboBox;
    EdtHost: TLabeledEdit;
    EdtUser: TLabeledEdit;
    EdtPassword: TLabeledEdit;
    Label1: TLabel;
    EdtPort: TSpinEdit;
    EdtConfValues: TValueListEditor;
    EdtDatabaseFilter: TLabeledEdit;
    EdtTraceName: TLabeledEdit;
    DlgSaveConf: TSaveDialog;
    Label2: TLabel;
    procedure BtnOKClick(Sender: TObject);
    procedure BtnSaveConfClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FConfig: TTraceConfig;
    procedure SetConfig(Value: TTraceConfig);
    procedure UpdateConfig;
  public
    property Config: TTraceConfig read FConfig write SetConfig;
  end;

var
  FrmEditTrace: TFrmEditTrace;

implementation

uses
  AppServices,
  TraceFiles;

{$R *.lfm}

{ TFrmEditTrace }

procedure TFrmEditTrace.FormCreate(Sender: TObject);
begin
  EdtConfValues.FixedCols := 1;
  EdtConfValues.Strings.Clear;
end;

procedure TFrmEditTrace.BtnOKClick(Sender: TObject);
begin
  UpdateConfig;
end;

procedure TFrmEditTrace.BtnSaveConfClick(Sender: TObject);
begin
  UpdateConfig;
  DlgSaveConf.Filter := TTraceConfFile.GetDlgFilter;
  DlgSaveConf.InitialDir := SysUtils.GetCurrentDir;
  DlgSaveConf.FileName := Config.TraceName + TraceConfExt;
  if DlgSaveConf.Execute then
    TTraceConfFile.Save(DlgSaveConf.FileName, Config);
end;

procedure TFrmEditTrace.SetConfig(Value: TTraceConfig);
var
  i, Index: integer;
  Param: TTraceConfigParam;
begin
  Assert(Value <> nil, 'Configuration provided is null');
  FConfig := Value;
  EdtTraceName.Text := FConfig.TraceName;
  EdtHost.Text := FConfig.HostName;
  EdtPort.Value := FConfig.Port;
  i := EdtVersion.Items.IndexOf(FConfig.Version);
  if i >= 0 then
    EdtVersion.ItemIndex := i
  else
    EdtVersion.ItemIndex := 0;
  EdtUser.Text := FConfig.UserName;
  EdtPassword.Text := FConfig.Password;
  EdtDatabaseFilter.Text := FConfig.DatabaseFilter;
  EdtConfValues.Strings.Clear;

  for i := 0 to FConfig.Params.Count - 1 do
  begin
    Param := FConfig.Params[i];
    Index := EdtConfValues.Strings.AddObject(
      Param.Name + '=' + Param.Value, Param);
    case Param.ParamType of
      ptList, ptBoolean:
      begin
        EdtConfValues.ItemProps[Index].EditStyle := esPickList;
        EdtConfValues.ItemProps[Index].PickList.AddStrings(Param.PickList);
      end;
      else
        EdtConfValues.ItemProps[Index].EditStyle := esSimple;
    end;
  end;
end;

procedure TFrmEditTrace.UpdateConfig;
var
  i: integer;
  Param: TTraceConfigParam;
begin
  if FConfig = nil then
    Exit;
  FConfig.TraceName := EdtTraceName.Text;
  FConfig.HostName := EdtHost.Text;
  FConfig.Port := EdtPort.Value;
  FConfig.Version := EdtVersion.Text;
  FConfig.UserName := EdtUser.Text;
  FConfig.Password := EdtPassword.Text;
  FConfig.DatabaseFilter := EdtDatabaseFilter.Text;

  for i := 0 to EdtConfValues.Strings.Count - 1 do
  begin
    //TODO Error with EdtConfValues.Values[] vs EdtConfValues.Strings.Values[]
    //GetLog.Debug('%d: %s. Key: %s, value: %s', [i, EdtConfValues.Strings[i], EdtConfValues.Strings.Names[i], EdtConfValues.Values[EdtConfValues.Strings.Names[i]]]);
    Param := TTraceConfigParam(EdtConfValues.Strings.Objects[i]);
    Assert(Param <> nil, 'Param is nil: ' + EdtConfValues.Strings[i]);
    Param.Value := EdtConfValues.Strings.Values[EdtConfValues.Strings.Names[i]];
  end;
end;

end.

