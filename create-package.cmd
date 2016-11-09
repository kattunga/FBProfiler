@echo off

set PATH=C:\lazarus;C:\Program Files\7-Zip;%PATH%
set archive_name=fbprofiler.zip

lazbuild fbprofiler.lpr
if errorlevel 1 goto fail
lazbuild fbprofilercmd.lpr
if errorlevel 1 goto fail

del /q %archive_name%

7z a %archive_name% @create-package.lst
if errorlevel 1 goto fail

echo OK
exit /b 0

:fail
echo FAILED
exit /b 1
