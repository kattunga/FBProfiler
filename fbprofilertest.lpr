program fbprofilertest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, FBTraceEventsTest, fbtraceevents;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

