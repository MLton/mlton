#ifndef _BASIS_CONSTANTS_H_
#define _BASIS_CONSTANTS_H_

#include <sys/time.h>
#if (defined (__linux__))
#include <sys/ptrace.h>
#endif
#include <sys/socket.h>

#include "gc.h"

/* ------------------------------------------------- */
/*                       Array                       */
/* ------------------------------------------------- */

#define Array_maxLen GC_MAX_ARRAY_LENGTH

/* ------------------------------------------------- */
/*                      Itimer                       */
/* ------------------------------------------------- */

#define Itimer_prof ITIMER_PROF
#define Itimer_real ITIMER_REAL
#define Itimer_virtual ITIMER_VIRTUAL

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

#if (defined (__CYGWIN__))
#define MLton_hostType 0
#elif (defined (__FreeBSD__))
#define MLton_hostType 1
#elif (defined (__linux__))
#define MLton_hostType 2
#else
#error MLton_hostType not defined
#endif
#define MLton_isLittleEndian TRUE

/* ------------------------------------------------- */
/*                      Ptrace                       */
/* ------------------------------------------------- */

#if (defined (__linux__))

/* Nothing to do -- everything comes from sys/ptrace.h. */

#elif (defined (__CYGWIN__) || defined (__FreeBSD__))

#define PTRACE_BOGUS 0xFFFFFFFF
#define PTRACE_SYSCALL PTRACE_BOGUS
#define PTRACE_SETFPREGS PTRACE_BOGUS
#define PTRACE_GETFPREGS PTRACE_BOGUS
#define PTRACE_SETREGS PTRACE_BOGUS
#define PTRACE_GETREGS PTRACE_BOGUS
#define PTRACE_DETACH PTRACE_BOGUS
#define PTRACE_ATTACH PTRACE_BOGUS
#define PTRACE_SINGLESTEP PTRACE_BOGUS
#define PTRACE_KILL PTRACE_BOGUS
#define PTRACE_CONT PTRACE_BOGUS
#define PTRACE_POKEDATA PTRACE_BOGUS
#define PTRACE_POKETEXT PTRACE_BOGUS
#define PTRACE_PEEKDATA PTRACE_BOGUS
#define PTRACE_PEEKTEXT PTRACE_BOGUS
#define PTRACE_TRACEME PTRACE_BOGUS

#else

#error PTRACE_ constants not defined

#endif

#define Ptrace_TRACEME PTRACE_TRACEME
#define Ptrace_PEEKTEXT PTRACE_PEEKTEXT
#define Ptrace_PEEKDATA PTRACE_PEEKDATA
#define Ptrace_PEEKUSR PTRACE_PEEKUSR
#define Ptrace_POKETEXT PTRACE_POKETEXT
#define Ptrace_POKEDATA PTRACE_POKEDATA
#define Ptrace_POKEUSR PTRACE_POKEUSR
#define Ptrace_CONT PTRACE_CONT
#define Ptrace_KILL PTRACE_KILL
#define Ptrace_SINGLESTEP PTRACE_SINGLESTEP
#define Ptrace_ATTACH PTRACE_ATTACH
#define Ptrace_DETACH PTRACE_DETACH
#define Ptrace_GETREGS PTRACE_GETREGS
#define Ptrace_SETREGS PTRACE_SETREGS
#define Ptrace_GETFPREGS PTRACE_GETFPREGS
#define Ptrace_SETFPREGS PTRACE_SETFPREGS
#define Ptrace_SYSCALL PTRACE_SYSCALL

#endif /* #ifndef _BASIS_CONSTANTS_H_ */
