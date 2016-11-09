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

unit FormSearchTrace;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, Buttons, ActnList, DMMain;

type

  { TFrmSearchTrace }

  TFrmSearchTrace = class(TForm)
    ActionOK: TAction;
    ActionList1: TActionList;
    BtnCancel: TBitBtn;
    BtnOK: TBitBtn;
    ChkDuration: TCheckBox;
    ChkPlan: TCheckBox;
    ChkSQL: TCheckBox;
    EdtDurationMax: TSpinEdit;
    EdtDurationMin: TSpinEdit;
    EdtPlan: TEdit;
    EdtSQL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure ActionList1Update(AAction: TBasicAction; var Handled: Boolean);
    procedure ActionOKExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure InitByFilter(const Filter: TTraceFilter);
    procedure SaveFilter(Filter: TTraceFilter);
  public
    { public declarations }
  end;

var
  FrmSearchTrace: TFrmSearchTrace;

implementation

{$R *.lfm}

{ TFrmSearchTrace }

procedure TFrmSearchTrace.InitByFilter(const Filter: TTraceFilter);
begin
  ChkDuration.Checked := Filter.DurationMsFilter;
  EdtDurationMin.Value := Filter.DurationMsMin;
  EdtDurationMax.Value := Filter.DurationMsMax;
  ChkPlan.Checked := Filter.PlanFilter;
  EdtPlan.Text := Filter.PlanExpr;
  ChkSQL.Checked := Filter.SQLFilter;
  EdtSQL.Text := Filter.SQLExpr;
end;

procedure TFrmSearchTrace.SaveFilter(Filter: TTraceFilter);
begin
  Filter.DurationMsFilter := ChkDuration.Checked;
  Filter.DurationMsMin := EdtDurationMin.Value;
  Filter.DurationMsMax := EdtDurationMax.Value;
  Filter.PlanFilter := ChkPlan.Checked;
  Filter.PlanExpr := EdtPlan.Text;
  Filter.SQLFilter := ChkSQL.Checked;
  Filter.SQLExpr := EdtSQL.Text;
end;

procedure TFrmSearchTrace.FormCreate(Sender: TObject);
begin
  if DataModuleMain.CurrTraceFilter = nil then
    DataModuleMain.CurrTraceFilter := TTraceFilter.Create;
  InitByFilter(DataModuleMain.CurrTraceFilter);
end;

procedure TFrmSearchTrace.ActionOKExecute(Sender: TObject);
begin
  SaveFilter(DataModuleMain.CurrTraceFilter);
end;

procedure TFrmSearchTrace.ActionList1Update(AAction: TBasicAction;
  var Handled: Boolean);
begin
  EdtDurationMin.Enabled := ChkDuration.Checked;
  EdtDurationMax.Enabled := ChkDuration.Checked;
  EdtPlan.Enabled := ChkPlan.Checked;
  EdtSQL.Enabled := ChkSQL.Checked;
  Handled := true;
end;

end.

