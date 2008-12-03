@echo off
if "%CMDEXTVERSION%"=="" (
  echo Batch file execution of MLton not supported without command extensions
  goto :eof
)

setlocal

rem Use magic from the internet to get the parent directory
for %%? in ("%~dp0..") do set dir=%%~f?
set lib=%dir%\lib\MLton
set bin=%dir%\bin


if not exist "%bin%" (
  echo MLton directory "%bin%" does not exist
  goto :eof
)

if not exist "%lib%" (
  echo MLton library directory "%lib%" does not exist
  goto :eof
)

set cc=%bin%\gcc.exe
if not exist "%cc%" (
  echo GCC compiler "%cc%" does not exist
  goto :eof
)

rem gcc needs to be pathed to find as, ld, etc
set PATH=%bin%;%PATH%

rem Check if invoked with special option (via explorer).
set pause=
if "%1" == "-pause" (
  set pause=yes
  shift
)

rem Put runtime arguments to %rargs% and the rest to %args%.
set args=
set rargs=
if not "%1" == "@MLton" goto args_loop
:rargs_loop
shift
if "%~1" == "" (
  echo @MLton missing --
  goto :eof
)
if "%1" == "--" (
  shift
  goto :args_loop
)
set rargs=%rargs% %1
goto :rargs_loop
:args_loop
if "%~1" == "" goto args_done
set args=%args% %1
shift
goto args_loop
:args_done

set world=%lib%\world.mlton
set mlton=%lib%\mlton-compile.exe

set ccopts=-O1 -fno-strict-aliasing -fomit-frame-pointer -w
set ccopts=%ccopts% -fno-strength-reduce -fschedule-insns -fschedule-insns2
set ccopts=%ccopts% -malign-functions=5 -malign-jumps=2 -malign-loops=2
set linkopts=-lm -lgmp -lws2_32 -lkernel32 -lpsapi -lnetapi32 -lwinmm -Wl,--enable-stdcall-fixup

"%mlton%" @MLton load-world "%world%" ram-slop 0.5 %rargs% -- "%lib%" -cc "%cc%" -ar-script "%bin%\static-library.bat" -cc-opt-quote "-I%lib%\include" -cc-opt "%ccopts%" -mlb-path-map "%lib%\mlb-path-map" -link-opt "%linkopts%" %args%
set retval=%errorlevel%

if "%pause%" == "yes" pause
exit /b %retval%
