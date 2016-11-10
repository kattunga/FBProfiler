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

unit FormAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TFrmAbout }

  TFrmAbout = class(TForm)
    BtnClose: TButton;
    Image2: TImage;
    Info: TStaticText;
    Label1: TLabel;
    Label2: TLabel;
    LblBelairLink: TLabel;
    LblBelairLink1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LblBelairLinkClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FrmAbout: TFrmAbout;

implementation

uses
  VersionTypes,
  LCLIntf,
  AppServices;

{$R *.lfm}

{ TFrmAbout }

procedure TFrmAbout.FormCreate(Sender: TObject);
var
  VI: TVersionFixedInfo;
begin
  VI := GetVersionInfo;
  Info.Caption := Application.Title + LineEnding +
    Format('v.%d.%d.%d build %d',
           [VI.FileVersion[0], VI.FileVersion[1], VI.FileVersion[2], VI.FileVersion[3]]) + LineEnding;
end;

procedure TFrmAbout.LblBelairLinkClick(Sender: TObject);
begin
  OpenURL(LblBelairLink.Caption);
end;

end.

