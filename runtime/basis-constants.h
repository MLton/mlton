#ifndef _BASIS_CONSTANTS_H_
#define _BASIS_CONSTANTS_H_

/* ------------------------------------------------- */
/*                       Array                       */
/* ------------------------------------------------- */

#include "gc.h"
#define Array_maxLen GC_MAX_ARRAY_LENGTH

/* ------------------------------------------------- */
/*                      Itimer                       */
/* ------------------------------------------------- */

#include <sys/time.h>

#define Itimer_prof ITIMER_PROF
#define Itimer_real ITIMER_REAL
#define Itimer_virtual ITIMER_VIRTUAL

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

#ifndef MLton_debug
#define MLton_debug FALSE
#endif

#define MLton_isLittleEndian TRUE

/* ------------------------------------------------- */
/*                      Ptrace                       */
/* ------------------------------------------------- */

#include <sys/ptrace.h>

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

/* ------------------------------------------------- */
/*                       Real                        */
/* ------------------------------------------------- */

/* returned by Real_class */
#define Real_Class_nanQuiet 0
#define Real_Class_nanSignalling 1
#define Real_Class_inf 2
#define Real_Class_zero 3
#define Real_Class_normal 4
#define Real_Class_subnormal 5

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

#include <sys/socket.h>

#define Socket_shutdownRead SHUT_RD
#define Socket_shutdownWrite SHUT_WR
#define Socket_shutdownReadWrite SHUT_RDWR

#endif /* #ifndef _BASIS_CONSTANTS_H_ */
