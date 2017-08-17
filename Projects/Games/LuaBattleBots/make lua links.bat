@echo off

cd %~dp0
call :create_links Debug Win64
call :create_links DebugConsole Win64
call :create_links Release Win64
call :create_links Debug Win32
call :create_links DebugConsole Win32
call :create_links Release Win32
pause
exit

:create_links
if not exist "%2\%1" (
mkdir "%2\%1"
echo + %2\%1
)
mklink "%2\%1\lua53.dll" "..\..\..\..\..\Libraries\Lua\Lua 5.3.4 %2\lua53.dll"
goto :eof