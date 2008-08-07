@echo off
if "%CMDEXTVERSION%"=="" goto :downlevel

rem %0 contains the name of this batch file, before the path was searched
rem But we can use the %~dp0 call-parameter syntax to find out what drive and directory it lives on
setlocal
call :setdir %~dp0 "%*"

if not exist %dir% (
  echo MLton directory %dir% does not exist
  goto :end
) 

set lib=%dir%\lib\MLton
if not exist %lib% (
  echo MLton library directory %lib% does not exist
  goto :end
)

set cc=%dir%\bin\gcc.exe
if not exist %cc% (
  echo GCC compiler %cc% does not exist
  goto :end
)

set world=%lib%\world.mlton
set mlton=%lib%\mlton-compile.exe

set ccopts=-O1 -fno-strict-aliasing -fomit-frame-pointer -w
set ccopts=%ccopts% -fno-strength-reduce -fschedule-insns -fschedule-insns2
set ccopts=%ccopts% -malign-functions=5 -malign-jumps=2 -malign-loops=2
set linkopts=--lm -lgmp -lws2_32 -lkernel32 -lpsapi -lnetapi32

%mlton% @MLton load-world %world% ram-slop 0.5 -- %lib% -cc %cc% -cc-opt-quote "-I%lib%\include" -cc-opt "%ccopts%" -mlb-path-map "%lib%\mlb-path-map" -link-opt "%linkopts%" %*
goto :eof

:setdir
set dir=%1%..\
GOTO :eof

:downlevel
echo Batch file execution of MLton not supported without command extensions
goto :end

:end
