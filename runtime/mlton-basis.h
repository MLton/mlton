#ifndef _MLTON_BASIS_H_
#define _MLTON_BASIS_H_

/* Here are the types that the abstract machine deals with.
 * See backend/mtype.sig.
 */
typedef char Char;
typedef double Double;
typedef int Int;
typedef char *Pointer;
typedef unsigned int Word;

/* Here are some type abbreviations for abstract machine types. */
typedef Int Bool;
typedef Word Cpointer;
typedef Word Cstring;
typedef Pointer Thread;
typedef Word CstringArray;
typedef Word NullString;

/* ------------------------------------------------- */
/*                       Array                       */
/* ------------------------------------------------- */

Int Array_numElements(Pointer p);

/* ------------------------------------------------- */
/*                         C                         */
/* ------------------------------------------------- */

Char C_CS_sub(Cstring s, Int i);
void C_CS_update(Cstring s, Int i, Char c);
Cstring C_CSS_sub(CstringArray a, Int i);

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

void Debug_enter(Pointer name);
void Debug_leave(Pointer name);

/* ------------------------------------------------- */
/*                        GC                         */
/* ------------------------------------------------- */

void GC_setMessages(Int b);
void GC_setSummary(Int b);

/* ------------------------------------------------- */
/*                     IEEEReal                      */
/* ------------------------------------------------- */

void IEEEReal_setRoundingMode(Int mode);
Int IEEEReal_getRoundingMode();

/* ------------------------------------------------- */
/*                        Int                        */
/* ------------------------------------------------- */

Bool Int_addOverflow(int n1, int n2, int *res);
Bool Int_mulOverflow(int n1, int n2, int *res);
Bool Int_negOverflow(int n, int *res);
Bool Int_subOverflow(int n1, int n2, int *res);
Int Int_quot(Int numerator, Int denominator);
Int Int_rem(Int numerator, Int denominator);

/* ------------------------------------------------- */
/*                      Itimer                       */
/* ------------------------------------------------- */

void Itimer_set(Int which,
		Int interval_tv_sec, Int interval_tv_usec,
		Int value_tv_sec, Int value_tv_usec);

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

/* print a bug message and exit (2) */
void MLton_bug (Pointer msg);
/* halt the machine */
void MLton_exit (Int status);
Word MLton_random ();
Word MLton_size (Pointer p);

/* ------------------------------------------------- */
/*                        OS                         */
/* ------------------------------------------------- */

Cstring OS_FileSys_tmpnam();

/* ------------------------------------------------- */
/*                     PackReal                      */
/* ------------------------------------------------- */

Double PackReal_subVec(Pointer v, Int offset);
void PackReal_update(Pointer a, Int offset, Double r);

/* ------------------------------------------------- */
/*                      Ptrace                       */
/* ------------------------------------------------- */

Int Ptrace_ptrace2(Int request, Int pid);
/* data is a word ref */
Int Ptrace_ptrace4(Int request, Int pid, Word addr, Pointer data);

/* ------------------------------------------------- */
/*                       Real                        */
/* ------------------------------------------------- */

extern Double Real_Math_e;
extern Double Real_Math_pi;
extern Double Real_posInf;
extern Double Real_maxFinite;
extern Double Real_minNormalPos;
extern Double Real_minPos;

Int Real_class(Double d);
Int Real_isFinite(Double d);
Int Real_isNan(Double d);
Int Real_isNormal(Double d);
Int Real_isPositive(Double d);
Int Real_qequal(Double x1, Double x2);
double Real_round(Double d);
Int Real_signBit(Double d);

/* ------------------------------------------------- */
/*                      Rlimit                       */
/* ------------------------------------------------- */

#include <sys/resource.h>

#define MLton_Rlimit_cpuTime RLIMIT_CPU
#define MLton_Rlimit_coreFileSize RLIMIT_CORE
#define MLton_Rlimit_dataSize RLIMIT_DATA
#define MLton_Rlimit_fileSize RLIMIT_FSIZE
#define MLton_Rlimit_lockedInMemorySize RLIMIT_MEMLOCK
#define MLton_Rlimit_numFiles RLIMIT_NOFILE
#define MLton_Rlimit_numProcesses RLIMIT_NPROC
#define MLton_Rlimit_residentSetSize RLIMIT_RSS
#define MLton_Rlimit_stackSize RLIMIT_STACK
#define MLton_Rlimit_virtualMemorySize RLIMIT_AS

#define MLton_Rlimit_infinity RLIM_INFINITY

typedef Word Rlimit;
typedef Int Resource;

Int MLton_Rlimit_get(Resource r);
Rlimit MLton_Rlimit_getHard();
Rlimit MLton_Rlimit_getSoft();
Int MLton_Rlimit_set(Resource r, Rlimit hard, Rlimit soft);

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

Word Socket_Addr_address();
Int Socket_Addr_port();
Cstring Socket_Host_name();
Int Socket_Host_getByAddress(Word addr);
Int Socket_Host_getByName(Cstring name);
Int Socket_accept(Int sl);
Int Socket_connect(Pointer host, Int port);
Int Socket_listen(Pointer port, Pointer resultSocket);
Int Socket_Shutdown(Int fd, Int how);

/* ------------------------------------------------- */
/*                       Stdio                       */
/* ------------------------------------------------- */

void Stdio_print(Pointer s);
Int Stdio_sprintf(Pointer buf, Pointer fmt, Double x);

/* ------------------------------------------------- */
/*                      String                       */
/* ------------------------------------------------- */

int String_equal(char * s1, char * s2);

/* ------------------------------------------------- */
/*                      Thread                       */
/* ------------------------------------------------- */

void Thread_atomicBegin();
void Thread_atomicEnd();
Thread Thread_current();
Thread Thread_saved();
void Thread_setHandler(Thread t);
void Thread_switchTo(Thread t);

/* ------------------------------------------------- */
/*                       Time                        */
/* ------------------------------------------------- */

Int Time_gettimeofday();
Int Time_sec();
Int Time_usec();

/* ------------------------------------------------- */
/*                       Word8                       */
/* ------------------------------------------------- */

Char Word8_arshiftAsm(Char w, Word s);

/* ------------------------------------------------- */
/*                      Word32                       */
/* ------------------------------------------------- */

Word Word32_arshiftAsm(Word w, Word s);

#endif /* #ifndef _MLTON_BASIS_H_ */
