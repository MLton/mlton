/* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */
#ifndef _MLTON_BASIS_H_
#define _MLTON_BASIS_H_

#if (defined (__FreeBSD__))
#include <sys/time.h>
#endif
#include <sys/resource.h>

#include "types.h"

/* Here are some type abbreviations for abstract machine types. */
typedef Word Cpointer;
typedef Word Cstring;
typedef Pointer Thread;
typedef Word CstringArray;
typedef Word NullString;

/* ------------------------------------------------- */
/*                       Array                       */
/* ------------------------------------------------- */

Int Array_numElements (Pointer p);

/* ------------------------------------------------- */
/*                    CommandLine                    */
/* ------------------------------------------------- */

/* These are initialized by MLton_init. */
extern Int CommandLine_argc;
extern CstringArray CommandLine_argv;
extern Cstring CommandLine_commandName;

/* ------------------------------------------------- */
/*                       Date                        */
/* ------------------------------------------------- */

Int Date_Tm_sec();
Int Date_Tm_min();
Int Date_Tm_hour();
Int Date_Tm_mday();
Int Date_Tm_mon();
Int Date_Tm_year();
Int Date_Tm_wday();
Int Date_Tm_yday();
Int Date_Tm_isdst();
void Date_Tm_setSec(Int x);
void Date_Tm_setMin(Int x);
void Date_Tm_setHour(Int x);
void Date_Tm_setMday(Int x);
void Date_Tm_setMon(Int x);
void Date_Tm_setYear(Int x);
void Date_Tm_setWday(Int x);
void Date_Tm_setYday(Int x);
void Date_Tm_setIsdst(Int x);

Cstring Date_ascTime();
void Date_gmTime(Pointer p);
Int Date_localOffset();
void Date_localTime(Pointer p);
Int Date_mkTime();
Int Date_strfTime(Pointer buf, Int n, NullString fmt);

/* ------------------------------------------------- */
/*                       Debug                       */
/* ------------------------------------------------- */

void Debug_enter (Pointer name);
void Debug_leave (Pointer name);

/* ------------------------------------------------- */
/*                        GC                         */
/* ------------------------------------------------- */

void GC_setMessages (Int b);
void GC_setSummary (Int b);

/* ------------------------------------------------- */
/*                     IEEEReal                      */
/* ------------------------------------------------- */

void IEEEReal_setRoundingMode (Int mode);
Int IEEEReal_getRoundingMode ();

/* ------------------------------------------------- */
/*                      Itimer                       */
/* ------------------------------------------------- */

void Itimer_set (Int which,
			Int interval_tv_sec, Int interval_tv_usec,
			Int value_tv_sec, Int value_tv_usec);

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

/* print a bug message and exit (2) */
void MLton_bug (Pointer msg);

Int MLton_errno ();
/* halt the machine */
void MLton_exit (Int status);
Word MLton_random ();
Word MLton_size (Pointer p);

void MLton_Profile_Data_free (Pointer d);
Pointer MLton_Profile_Data_malloc (void);
void MLton_Profile_Data_write (Pointer data, Word fd);

Pointer MLton_Profile_current (void);
void MLton_Profile_done ();
void MLton_Profile_setCurrent (Pointer d);

#if (defined (__CYGWIN__))
Int MLton_Process_spawne (NullString p, Pointer a, Pointer e);
Int MLton_Process_spawnp (NullString p, Pointer a);
#endif

/* ------------------------------------------------- */
/*                        OS                         */
/* ------------------------------------------------- */

Cstring OS_FileSys_tmpnam ();
Int OS_IO_poll (Int *fds, Word *eventss, Int n, Int timeout, Word *reventss);

/* ------------------------------------------------- */
/*                     PackReal                      */
/* ------------------------------------------------- */

Real64 PackReal_subVec (Pointer v, Int offset);
void PackReal_update (Pointer a, Int offset, Real64 r);

/* ------------------------------------------------- */
/*                      Ptrace                       */
/* ------------------------------------------------- */

Int Ptrace_ptrace2 (Int request, Int pid);
/* data is a word ref */
Int Ptrace_ptrace4 (Int request, Int pid, Word addr, Pointer data);

/* ------------------------------------------------- */
/*                      Rlimit                       */
/* ------------------------------------------------- */

#if (defined (__CYGWIN__) || defined (__sun__))
#define RLIMIT_BOGUS 0xFFFFFFFF
#define RLIMIT_RSS RLIMIT_BOGUS
#define RLIMIT_NPROC RLIMIT_BOGUS
#define RLIMIT_MEMLOCK RLIMIT_BOGUS
#endif

#define MLton_Rlimit_cpuTime RLIMIT_CPU
#define MLton_Rlimit_coreFileSize RLIMIT_CORE
#define MLton_Rlimit_dataSize RLIMIT_DATA
#define MLton_Rlimit_fileSize RLIMIT_FSIZE
#define MLton_Rlimit_lockedInMemorySize RLIMIT_MEMLOCK
#define MLton_Rlimit_numFiles RLIMIT_NOFILE
#define MLton_Rlimit_numProcesses RLIMIT_NPROC
#define MLton_Rlimit_residentSetSize RLIMIT_RSS
#define MLton_Rlimit_stackSize RLIMIT_STACK
#if (defined (__FreeBSD__) || defined (__NetBSD__))
#define MLton_Rlimit_virtualMemorySize RLIMIT_DATA
#elif (defined (__CYGWIN__) || defined (__linux__) || defined (__sun__))
#define MLton_Rlimit_virtualMemorySize RLIMIT_AS
#else
#error MLton_Rlimit_virtualMemorySize not defined
#endif

#define MLton_Rlimit_infinity RLIM_INFINITY

typedef Word Rlimit;
typedef Int Resource;

Int MLton_Rlimit_get (Resource r);
Rlimit MLton_Rlimit_getHard ();
Rlimit MLton_Rlimit_getSoft ();
Int MLton_Rlimit_set (Resource r, Rlimit hard, Rlimit soft);

/* ------------------------------------------------- */
/*                       Stdio                       */
/* ------------------------------------------------- */

void Stdio_print (Pointer s);
Int Stdio_sprintf (Pointer buf, Pointer fmt, Real64 x);

/* ------------------------------------------------- */
/*                      String                       */
/* ------------------------------------------------- */

int String_equal (char * s1, char * s2);

/* ------------------------------------------------- */
/*                      Thread                       */
/* ------------------------------------------------- */

Thread Thread_current ();
void Thread_finishHandler ();
void Thread_resetSignals ();
Thread Thread_saved ();
void Thread_setHandler (Thread t);
void Thread_startHandler ();
void Thread_switchTo (Thread t, Word ensureBytesFree);

/* ------------------------------------------------- */
/*                       Time                        */
/* ------------------------------------------------- */

Int Time_gettimeofday ();
Int Time_sec ();
Int Time_usec ();

/* ------------------------------------------------- */
/*                       Word8                       */
/* ------------------------------------------------- */

Char Word8_arshiftAsm (Char w, Word s);

/* ------------------------------------------------- */
/*                      Word32                       */
/* ------------------------------------------------- */

Word Word32_arshiftAsm (Word w, Word s);

#endif /* #ifndef _MLTON_BASIS_H_ */
