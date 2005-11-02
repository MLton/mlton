@echo off
set lib=c:\MLton\lib
set cc=c:\MinGW\bin\gcc.exe

set world=%lib%\world.mlton
set mlton=%lib%\mlton-compile.exe

set ccopts=-I%lib%\include -O1 -fno-strict-aliasing -fomit-frame-pointer -w
set ccopts=%ccopts% -fno-strength-reduce -fschedule-insns -fschedule-insns2
set ccopts=%ccopts% -malign-functions=5 -malign-jumps=2 -malign-loops=2 -mtune=pentium4
set linkopts=-L%lib%\lib -lgdtoa -lgmp -lws2_32 -lkernel32 -lpsapi -lnetapi32

%mlton% @MLton load-world %world% -- %lib% -cc %cc% -cc-opt "%ccopts%" -link-opt "%linkopts%" %1 %2 %3 %4 %5 %6 %7 %8 %9
