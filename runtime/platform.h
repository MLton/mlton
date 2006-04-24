/* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _PLATFORM_H_
#define _PLATFORM_H_

#define _ISOC99_SOURCE
#define _BSD_SOURCE

/* Only enable _POSIX_C_SOURCE on platforms that don't have broken system
 * headers.
 */
#if (defined (__linux__))
#define _POSIX_C_SOURCE 200112L
#endif

#include <sys/types.h> // lots of includes depend on this
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>
#include <utime.h>

/* C99-specific headers */
#include <inttypes.h>

/* On FreeBSD and OpenBSD the default gmp.h is installed in /usr/include, 
 * but that is version 2.  We want gmp version 4, which is installed in 
 * /usr/local/include, and is ensured to exist because it is required by the
 * MLton package.
 * On NetBSD, we want gmp to be installed into the pkg tree (which represents
 * the FreeBSD ports tree). For now we use the same method as in the FreeBSD
 * case, but we note that this should be changed so the makefile provides the
 * correct -I flags to the compiler.
 * On MacOS X, many users will use fink to install gmp, in which case gmp.h
 * will be installed in /sw/include.
 */
#include "gmp.h"

#include "assert.h"

#if (defined (__APPLE_CC__))
#define __Darwin__
#endif

#if (defined (__CYGWIN__))
#include "platform/cygwin.h"
#elif (defined (__Darwin__))
#include "platform/darwin.h"
#elif (defined (__FreeBSD__))
#include "platform/freebsd.h"
#elif (defined (__hpux__))
#include "platform/hpux.h"
#elif (defined (__linux__))
#include "platform/linux.h"
#elif (defined (__MINGW32__))
#include "platform/mingw.h"
#elif (defined (__NetBSD__))
#include "platform/netbsd.h"
#elif (defined (__OpenBSD__))
#include "platform/openbsd.h"
#elif (defined (__sun__))
#include "platform/solaris.h"
#else
#error unknown platform
#endif

#ifndef bool
#define bool    int                     /* boolean type */
#endif
#define uint    unsigned int            /* short names for unsigned types */
#define ulong   unsigned long
#define ullong  unsigned long long      /* GCC extension */
#define llong   long long               /* GCC extension */
#define uchar   unsigned char
#define ushort  unsigned short int
#define not     !                       /* logical negation operator */
#define and     &&                      /* logical conjunction */
#define or      ||                      /* logical disjunction */
#ifndef TRUE
#define TRUE    (0 == 0)
#endif
#ifndef FALSE
#define FALSE   (not TRUE)
#endif
#define loop    while (TRUE)            /* loop until break */
#define EOS     '\0'                    /* end-of-string char */
#ifndef NULL
#define NULL    0                       /* invalid pointer */
#endif

#define NEW(t, x)               x = (t)(smalloc (sizeof(*x)))
#define ARRAY(t, a, s)  a = (t)(scalloc (s, sizeof(*a)))
#define ARRAY_UNSAFE(t, a, s)   a = (t)(calloc (s, sizeof(*a)))

#define string char*

#define unless(p)       if (not (p))
#define until(p)        while (not (p))
#define cardof(a)       (sizeof(a) / sizeof(*(a)))
#define endof(a)        ((a) + cardof(a))
#define bitsof(a)       (sizeof(a) * 8)

#ifndef max
#define max(a, b) ((a)>(b)?(a):(b))
#endif

#ifndef min
#define min(a, b) ((a)<(b)?(a):(b))
#endif

#ifndef MLton_Platform_OS_host
#error MLton_Platform_OS_host not defined
#endif

#ifndef HAS_FPCLASSIFY
#error HAS_FPCLASSIFY not defined
#endif

#ifndef HAS_FEROUND
#error HAS_FEROUND not defined
#endif

#ifndef HAS_PTRACE
#error HAS_PTRACE not defined
#endif

#ifndef HAS_REMAP
#error HAS_REMAP not defined
#endif

#ifndef HAS_SIGALTSTACK
#error HAS_SIGALTSTACK not defined
#endif

#ifndef HAS_SIGNBIT
#error HAS_SIGNBIT not defined
#endif

#ifndef HAS_SPAWN
#error HAS_SPAWN not defined
#endif

#ifndef HAS_TIME_PROFILING
#error HAS_TIME_PROFILING not defined
#endif

#ifndef EXECVP
#define EXECVP execvp
#endif

#ifndef EXECVE
#define EXECVE execve
#endif

#if not HAS_FEROUND
#ifndef FE_TONEAREST
#define FE_TONEAREST 0
#endif
#ifndef FE_DOWNWARD
#define FE_DOWNWARD 1
#endif
#ifndef FE_UPWARD
#define FE_UPWARD 2
#endif
#ifndef FE_TOWARDZERO
#define FE_TOWARDZERO 3
#endif
#endif

#if not HAS_FPCLASSIFY
#ifndef FP_INFINITE
#define FP_INFINITE 1
#endif
#ifndef FP_NAN
#define FP_NAN 0
#endif
#ifndef FP_NORMAL
#define FP_NORMAL 4
#endif
#ifndef FP_SUBNORMAL
#define FP_SUBNORMAL 3
#endif
#ifndef FP_ZERO
#define FP_ZERO 2
#endif
#endif

/* If HAS_TIME_PROFILING, then you must define these. */
void *getTextStart ();
void *getTextEnd ();

#ifndef SPAWN_MODE
#define SPAWN_MODE 0
#endif

#ifndef INT_MIN
#define INT_MIN ((int)0x80000000)
#endif
#ifndef INT_MAX
#define INT_MAX ((int)0x7FFFFFFF)
#endif

enum {
        DEBUG_MEM = FALSE,
        DEBUG_SIGNALS = FALSE,
};

#include "types.h"
#include "gc.h"

/* ---------------------------------------------------------------- */
/*                        Utility libraries                         */
/* ---------------------------------------------------------------- */

string boolToString (bool b);
void decommit (void *base, size_t length);
/* issue error message and exit */
extern void die (char *fmt, ...)
                        __attribute__ ((format(printf, 1, 2)))
                        __attribute__ ((noreturn));
/* issue error message and exit.  Also print strerror(errno). */
extern void diee (char *fmt, ...)
                        __attribute__ ((format(printf, 1, 2)))
                        __attribute__ ((noreturn));
/*
 * fixedGetrusage() works just like getrusage().  We have a wrapper because on 
 * some platforms (e.g. Linux) we need to work around kernel bugs in getrusage.
 */
int fixedGetrusage (int who, struct rusage *rup);
bool heapRemap (GC_state s, GC_heap h, W32 desired, W32 minSize);
string intToCommaString (int n);
int mkdir2 (const char *pathname, mode_t mode);
void *mmapAnon (void *start, size_t length);
void release (void *base, size_t length);
void *remap (void *old,  size_t oldSize, size_t newSize);
void *scalloc (size_t nmemb, size_t size);
void sclose (int fd);
void setSigProfHandler (struct sigaction *sa);
void sfclose (FILE *file);
FILE *sfopen (char *fileName, char *mode);
void sfread (void *ptr, size_t size, size_t nmemb, FILE *file);
uint sfreadUint (FILE *file);
void sfwrite (void *ptr, size_t size, size_t nmemb, FILE *file);
/* showMem displays the virtual memory mapping to stdout.  
 * It is used to diagnose memory problems. 
 */
void showMem ();
void *smalloc (size_t length);
int smkstemp (char *template);
void *smmap (size_t length);
/* A super-safe mmap.
 *  Allocates a region of memory with dead zones at the high and low ends.
 *  Any attempt to touch the dead zone (read or write) will cause a
 *   segmentation fault.
 */
void *ssmmap (size_t length, size_t dead_low, size_t dead_high);
void swrite (int fd, const void *buf, size_t count);
void swriteUint (int fd, uint n);
/*
 * totalRam returns the amount of physical memory on the machine (in
 * bytes).  */
Word32 totalRam (GC_state s);
string uintToCommaString (uint n);
string ullongToCommaString (ullong n);

static inline bool isBigEndian(void) {
        union {
                Word16 x;
                Word8 y;
        } z;
        
        /* gcc optimizes the following code to just return the result. */
        z.x = 0xABCDU;
        if (z.y == 0xAB) return TRUE; /* big endian */
        if (z.y == 0xCD) return FALSE; /* little endian */
        die ("Could not detect endian --- neither big nor little!\n");
        return 0;
}

#define MLton_Platform_Arch_bigendian isBigEndian()

/* ---------------------------------------------------------------- */
/*                         MLton libraries                          */
/* ---------------------------------------------------------------- */

/* ------------------------------------------------- */
/*                       Array                       */
/* ------------------------------------------------- */

#define Array_maxLen GC_MAX_ARRAY_LENGTH
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

#define FE_NOSUPPORT -1
/* Can't handle undefined rounding modes with code like the following.
 *  #ifndef FE_TONEAREST
 *  #define FE_TONEAREST FE_NOSUPPORT
 *  #endif
 * On some platforms, FE_* are defined via an enum, not the preprocessor,
 * and hence don't show up as #defined.  In that case, the above code 
 * overwrites them.
 */
void IEEEReal_setRoundingMode (Int mode);
Int IEEEReal_getRoundingMode ();

/* ------------------------------------------------- */
/*                      IntInf                       */
/* ------------------------------------------------- */

/*
 * Third header word for bignums and strings.
 */
#define BIGMAGIC        GC_objectHeader (WORD32_VECTOR_TYPE_INDEX)
#define STRMAGIC        GC_objectHeader (STRING_TYPE_INDEX)

/*
 * Layout of bignums.  Note, the value passed around is a pointer to
 * the isneg member.
 */
typedef struct  bignum {
        uint    counter,        /* used by GC. */
                card,           /* one more than the number of limbs */
                magic,          /* BIGMAGIC */
                isneg;          /* iff bignum is negative */
        ulong   limbs[0];       /* big digits, least significant first */
}       bignum;

/* All of these routines modify the frontier in gcState.  They assume that 
 * there are bytes bytes free, and allocate an array to store the result
 * at the current frontier position.
 * Immediately after the bytesArg, they take a labelIndex arg.  This is an index
 * into the array used for allocation profiling, and the appropriate element
 * is incremented by the amount that the function moves the frontier.
 */
Pointer IntInf_add (Pointer lhs, Pointer rhs, uint bytes);
Pointer IntInf_andb (Pointer lhs, Pointer rhs, uint bytes);
Pointer IntInf_arshift (Pointer arg, uint shift, uint bytes);
Pointer IntInf_gcd (Pointer lhs, Pointer rhs, uint bytes);
Pointer IntInf_lshift (Pointer arg, uint shift, uint bytes);
Pointer IntInf_mul (Pointer lhs, Pointer rhs, uint bytes);
Pointer IntInf_neg (Pointer arg, uint bytes);
Pointer IntInf_notb (Pointer arg, uint bytes);
Pointer IntInf_orb (Pointer lhs, Pointer rhs, uint bytes);
Pointer IntInf_quot (Pointer num, Pointer den, uint bytes);
Pointer IntInf_rem (Pointer num, Pointer den, uint bytes);
Pointer IntInf_sub (Pointer lhs, Pointer rhs, uint bytes);
Pointer IntInf_toString (Pointer arg, int base, uint bytes);
Pointer IntInf_xorb (Pointer lhs, Pointer rhs, uint bytes);

Word IntInf_smallMul (Word lhs, Word rhs, Pointer carry);
Int IntInf_compare (Pointer lhs, Pointer rhs);
Bool IntInf_equal (Pointer lhs, Pointer rhs);

/* ------------------------------------------------- */
/*                      Itimer                       */
/* ------------------------------------------------- */

#define Itimer_prof ITIMER_PROF
#define Itimer_real ITIMER_REAL
#define Itimer_virtual ITIMER_VIRTUAL

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

/* ---------------------------------- */
/*           MLton.Platform           */
/* ---------------------------------- */

#if (defined (__alpha__))
#define MLton_Platform_Arch_host "alpha"
#elif (defined (__x86_64__))
#define MLton_Platform_Arch_host "amd64"
#elif (defined (__arm__))
#define MLton_Platform_Arch_host "arm"
#elif (defined (__hppa__))
#define MLton_Platform_Arch_host "hppa"
#elif (defined (__ia64__))
#define MLton_Platform_Arch_host "ia64"
#elif (defined (__m68k__))
#define MLton_Platform_Arch_host "m68k"
#elif (defined (__mips__))
#define MLton_Platform_Arch_host "mips"
#elif (defined (__ppc__)) || (defined (__powerpc__))
#define MLton_Platform_Arch_host "powerpc"
#elif (defined (__s390__))
#define MLton_Platform_Arch_host "s390"
#elif (defined (__sparc__))
#define MLton_Platform_Arch_host "sparc"
#elif (defined (__i386__))
#define MLton_Platform_Arch_host "x86"
#else
#error MLton_Platform_Arch_host not defined
#endif

extern Bool MLton_Platform_CygwinUseMmap;

/* ---------------------------------- */
/*           MLton.Profile            */
/* ---------------------------------- */

void MLton_Profile_Data_free (Pointer d);
Pointer MLton_Profile_Data_malloc (void);
void MLton_Profile_Data_write (Pointer data, Word fd);

Pointer MLton_Profile_current (void);
void MLton_Profile_done ();
void MLton_Profile_setCurrent (Pointer d);

/* ---------------------------------- */
/*           MLton.Process            */
/* ---------------------------------- */

Pid MLton_Process_cwait (Pid p, Pointer s);
Int MLton_Process_spawne (NullString p, Pointer a, Pointer e);
Int MLton_Process_spawnp (NullString p, Pointer a);

/* ---------------------------------- */
/*            MLton.Rlimit            */
/* ---------------------------------- */

#define RLIMIT_BOGUS 0xFFFFFFFF

#ifndef RLIMIT_RSS
#define RLIMIT_RSS RLIMIT_BOGUS
#endif
#ifndef RLIMIT_NPROC
#define RLIMIT_NPROC RLIMIT_BOGUS
#endif
#ifndef RLIMIT_MEMLOCK
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
#if (defined (RLIMIT_DATA))
#define MLton_Rlimit_virtualMemorySize RLIMIT_DATA
#elif (defined (RLIMIT_AS))
#define MLton_Rlimit_virtualMemorySize RLIMIT_AS
#else
#error MLton_Rlimit_virtualMemorySize not defined
#endif

#define MLton_Rlimit_infinity RLIM_INFINITY

Int MLton_Rlimit_get (Resource r);
Rlimit MLton_Rlimit_getHard ();
Rlimit MLton_Rlimit_getSoft ();
Int MLton_Rlimit_set (Resource r, Rlimit hard, Rlimit soft);

/* ------------------------------------------------- */
/*                      OS                           */
/* ------------------------------------------------- */

#define OS_IO_POLLIN POLLIN
#define OS_IO_POLLPRI POLLPRI
#define OS_IO_POLLOUT POLLOUT

Cstring OS_FileSys_tmpnam ();
Int OS_IO_poll (Int *fds, Word *eventss, Int n, Int timeout, Word *reventss);

/* ------------------------------------------------- */
/*                     PackReal                      */
/* ------------------------------------------------- */

Real64 PackReal_subVec (Pointer v, Int offset);
void PackReal_update (Pointer a, Int offset, Real64 r);

/* ------------------------------------------------- */
/*                       Posix                       */
/* ------------------------------------------------- */

/* ---------------------------------- */
/*            Posix.Error             */
/* ---------------------------------- */

void Posix_Error_clearErrno ();
Int Posix_Error_gettErrno ();
Cstring Posix_Error_strerror (Syserror n);

#define Posix_Error_acces EACCES
#define Posix_Error_again EAGAIN
#define Posix_Error_badf EBADF
#ifndef EBADMSG
#define EBADMSG 0
#endif
#define Posix_Error_badmsg EBADMSG
#define Posix_Error_busy EBUSY
#ifndef ECANCELED
#define ECANCELED 0
#endif
#define Posix_Error_canceled ECANCELED
#define Posix_Error_child ECHILD
#define Posix_Error_deadlk EDEADLK
#define Posix_Error_dom EDOM
#define Posix_Error_exist EEXIST
#define Posix_Error_fault EFAULT
#define Posix_Error_fbig EFBIG
#define Posix_Error_inprogress EINPROGRESS
#define Posix_Error_intr EINTR
#define Posix_Error_inval EINVAL
#define Posix_Error_io EIO
#define Posix_Error_isdir EISDIR
#define Posix_Error_loop ELOOP
#define Posix_Error_mfile EMFILE
#define Posix_Error_mlink EMLINK
#define Posix_Error_msgsize EMSGSIZE
#define Posix_Error_nametoolong ENAMETOOLONG
#define Posix_Error_nfile ENFILE
#define Posix_Error_nodev ENODEV
#define Posix_Error_noent ENOENT
#define Posix_Error_noexec ENOEXEC
#define Posix_Error_nolck ENOLCK
#define Posix_Error_nomem ENOMEM
#define Posix_Error_nospc ENOSPC
#define Posix_Error_nosys ENOSYS
#define Posix_Error_notdir ENOTDIR
#define Posix_Error_notempty ENOTEMPTY
#ifndef ENOTSUP
#define ENOTSUP 0
#endif
#define Posix_Error_notsup ENOTSUP
#define Posix_Error_notty ENOTTY
#define Posix_Error_nxio ENXIO
#define Posix_Error_perm EPERM
#define Posix_Error_pipe EPIPE
#define Posix_Error_range ERANGE
#define Posix_Error_rofs EROFS
#define Posix_Error_spipe ESPIPE
#define Posix_Error_srch ESRCH
#define Posix_Error_toobig E2BIG
#define Posix_Error_xdev EXDEV

/* ---------------------------------- */
/*           Posix.FileSys            */
/* ---------------------------------- */

#define Posix_FileSys_S_ifsock S_IFSOCK
#define Posix_FileSys_S_iflnk S_IFLNK
#define Posix_FileSys_S_ifreg S_IFREG
#define Posix_FileSys_S_ifblk S_IFBLK
#define Posix_FileSys_S_ifdir S_IFDIR
#define Posix_FileSys_S_ifchr S_IFCHR
#define Posix_FileSys_S_ififo S_IFIFO

#ifndef O_BINARY
#define O_BINARY 0
#endif

#ifndef O_TEXT
#define O_TEXT 0
#endif

#define Posix_FileSys_O_append O_APPEND
#define Posix_FileSys_O_binary O_BINARY
#define Posix_FileSys_O_creat O_CREAT
#define Posix_FileSys_O_excl O_EXCL
#define Posix_FileSys_O_noctty O_NOCTTY
#define Posix_FileSys_O_nonblock O_NONBLOCK
#if (defined (O_SYNC))
#define Posix_FileSys_O_sync O_SYNC
#else
#define Posix_FileSys_O_sync 0
#endif
#define Posix_FileSys_O_text O_TEXT
#define Posix_FileSys_O_trunc O_TRUNC
#define Posix_FileSys_o_rdonly O_RDONLY
#define Posix_FileSys_o_wronly O_WRONLY
#define Posix_FileSys_o_rdwr O_RDWR
#define Posix_FileSys_S_irwxu S_IRWXU
#define Posix_FileSys_S_irusr S_IRUSR
#define Posix_FileSys_S_iwusr S_IWUSR
#define Posix_FileSys_S_ixusr S_IXUSR
#define Posix_FileSys_S_irwxg S_IRWXG
#define Posix_FileSys_S_irgrp S_IRGRP
#define Posix_FileSys_S_iwgrp S_IWGRP
#define Posix_FileSys_S_ixgrp S_IXGRP
#define Posix_FileSys_S_irwxo S_IRWXO
#define Posix_FileSys_S_iroth S_IROTH
#define Posix_FileSys_S_iwoth S_IWOTH
#define Posix_FileSys_S_ixoth S_IXOTH
#define Posix_FileSys_S_isuid S_ISUID
#define Posix_FileSys_S_isgid S_ISGID

#define Posix_FileSys_R_OK R_OK
#define Posix_FileSys_W_OK W_OK
#define Posix_FileSys_X_OK X_OK
#define Posix_FileSys_F_OK F_OK

/* used by pathconf and fpathconf */
#define Posix_FileSys_CHOWN_RESTRICTED _PC_CHOWN_RESTRICTED
#define Posix_FileSys_LINK_MAX _PC_LINK_MAX
#define Posix_FileSys_MAX_CANON _PC_MAX_CANON
#define Posix_FileSys_MAX_INPUT _PC_MAX_INPUT
#define Posix_FileSys_NAME_MAX _PC_NAME_MAX
#define Posix_FileSys_NO_TRUNC _PC_NO_TRUNC
#define Posix_FileSys_PATH_MAX _PC_PATH_MAX
#define Posix_FileSys_PIPE_BUF _PC_PIPE_BUF
#define Posix_FileSys_VDISABLE _PC_VDISABLE

#if (defined (_PC_SYNC_IO))
#define Posix_FileSys_SYNC_IO _PC_SYNC_IO
#else
#define Posix_FileSys_SYNC_IO 0
#endif

#if (defined (_PC_ASYNC_IO))
#define Posix_FileSys_ASYNC_IO _PC_ASYNC_IO
#else
#define Posix_FileSys_ASYNC_IO 0
#define Posix_FileSys_PRIO_IO 0
#endif

#if (defined (_PC_PRIO_IO))
#define Posix_FileSys_PRIO_IO _PC_PRIO_IO
#else
#define Posix_FileSys_PRIO_IO 0
#endif

Int Posix_FileSys_Dirstream_closedir (Dirstream d);
Dirstream Posix_FileSys_DirStream_opendir (NullString p);
Cstring Posix_FileSys_Dirstream_readdir (Dirstream d);
void Posix_FileSys_Dirstream_rewinddir (Dirstream p);

Int Posix_FileSys_Stat_fstat (Fd f);
Int Posix_FileSys_Stat_lstat (NullString f);
Int Posix_FileSys_Stat_stat (NullString f);
Word Posix_FileSys_Stat_dev ();
Int Posix_FileSys_Stat_ino ();
Word Posix_FileSys_Stat_mode ();
Int Posix_FileSys_Stat_nlink ();
Word Posix_FileSys_Stat_uid ();
Word Posix_FileSys_Stat_gid ();
Word Posix_FileSys_Stat_rdev ();
Position Posix_FileSys_Stat_size ();
Int Posix_FileSys_Stat_atime ();
Int Posix_FileSys_Stat_mtime ();
Int Posix_FileSys_Stat_ctime ();

void Posix_FileSys_Utimbuf_setActime (Int x);
void Posix_FileSys_Utimbuf_setModTime (Int x);
Int Posix_FileSys_Utimbuf_utime (NullString s);

Int Posix_FileSys_access (NullString f, Word w);
Int Posix_FileSys_chdir (NullString p);
Int Posix_FileSys_chmod (NullString p, Mode m);
Int Posix_FileSys_chown (NullString p, Uid u, Gid g);
Int Posix_FileSys_fchmod (Fd f, Mode m);
Int Posix_FileSys_fchown (Fd f, Uid u, Gid g);
Int Posix_FileSys_fpathconf (Fd f, Int n);
Int Posix_FileSys_ftruncate (Fd f, Position n);
Cstring Posix_FileSys_getcwd (Pointer buf, Size n);
Int Posix_FileSys_link (NullString p1, NullString p2);
Int Posix_FileSys_mkdir (NullString p, Word w);
Int Posix_FileSys_mkfifo (NullString p, Word w);
Int Posix_FileSys_open (NullString p, Word w, Mode m);
Int Posix_FileSys_pathconf (NullString p, Int n);
Int Posix_FileSys_readlink (NullString p, Pointer b, Int);
Int Posix_FileSys_rename (NullString p1, NullString p2);
Int Posix_FileSys_rmdir (NullString p);
Int Posix_FileSys_symlink (NullString p1, NullString p2);
Word Posix_FileSys_umask (Word w);
Word Posix_FileSys_unlink (NullString p);

Bool Posix_FileSys_ST_isDir (Word w);
Bool Posix_FileSys_ST_isChr (Word w);
Bool Posix_FileSys_ST_isBlk (Word w);
Bool Posix_FileSys_ST_isReg (Word w);
Bool Posix_FileSys_ST_isFIFO (Word w);
Bool Posix_FileSys_ST_isLink (Word w);
Bool Posix_FileSys_ST_isSock (Word w);

/* ---------------------------------- */
/*              Posix.IO              */
/* ---------------------------------- */

#define Posix_IO_F_DUPFD F_DUPFD
#define Posix_IO_F_GETFD F_GETFD
#define Posix_IO_F_SETFD F_SETFD
#define Posix_IO_F_GETFL F_GETFL
#define Posix_IO_F_SETFL F_SETFL
#define Posix_IO_F_GETLK F_GETLK
#define Posix_IO_F_SETLK F_SETLK
#define Posix_IO_F_RDLCK F_RDLCK
#define Posix_IO_F_WRLCK F_WRLCK
#define Posix_IO_F_UNLCK F_UNLCK
#define Posix_IO_F_SETLKW F_SETLKW
#define Posix_IO_F_GETOWN F_GETOWN
#define Posix_IO_F_SETOWN F_SETOWN
#define Posix_IO_O_ACCMODE O_ACCMODE
#define Posix_IO_SEEK_SET SEEK_SET
#define Posix_IO_SEEK_CUR SEEK_CUR
#define Posix_IO_SEEK_END SEEK_END
#define Posix_IO_FD_cloexec FD_CLOEXEC

Int Posix_IO_FLock_fcntl (Fd f, Int cmd);
Int Posix_IO_FLock_type ();
Int Posix_IO_FLock_whence ();
Position Posix_IO_FLock_start ();
Position Posix_IO_FLock_len ();
Int Posix_IO_FLock_pid ();
void Posix_IO_FLock_setType (Int x);
void Posix_IO_FLock_setWhence (Int x);
void Posix_IO_FLock_setStart (Position x);
void Posix_IO_FLock_setLen (Position x);
void Posix_IO_FLock_setPid (Int x);

Int Posix_IO_close (Fd f);
Fd Posix_IO_dup (Fd f);
Fd Posix_IO_dup2 (Fd f1, Fd f2);
Int Posix_IO_fcntl2 (Fd f, Int i);
Int Posix_IO_fcntl3 (Fd f, Int i, Int j);
Int Posix_IO_fsync (Fd f);
Position Posix_IO_lseek (Fd f, Position i, Int j);
Int Posix_IO_pipe (Pointer fds);
Ssize Posix_IO_read (Fd fd, Pointer b, Int i, Size s);
void Posix_IO_setbin (Fd fd);
void Posix_IO_settext (Fd fd);
Ssize Posix_IO_write (Fd fd, Pointer b, Int i, Size s);

/* ---------------------------------- */
/*           Posix.ProcEnv            */
/* ---------------------------------- */

extern CstringArray Posix_ProcEnv_environ;

/* used by sysconf. */
#define Posix_ProcEnv_2_FORT_DEV _SC_2_FORT_DEV
#define Posix_ProcEnv_2_FORT_RUN _SC_2_FORT_RUN
#define Posix_ProcEnv_2_SW_DEV _SC_2_SW_DEV
#define Posix_ProcEnv_2_VERSION _SC_2_VERSION
#define Posix_ProcEnv_ARG_MAX _SC_ARG_MAX
#define Posix_ProcEnv_BC_BASE_MAX _SC_BC_BASE_MAX
#define Posix_ProcEnv_BC_DIM_MAX _SC_BC_DIM_MAX
#define Posix_ProcEnv_BC_SCALE_MAX _SC_BC_SCALE_MAX
#define Posix_ProcEnv_BC_STRING_MAX _SC_BC_STRING_MAX
#define Posix_ProcEnv_CHILD_MAX _SC_CHILD_MAX
#define Posix_ProcEnv_CLK_TCK _SC_CLK_TCK
#define Posix_ProcEnv_COLL_WEIGHTS_MAX _SC_COLL_WEIGHTS_MAX
#define Posix_ProcEnv_EXPR_NEST_MAX _SC_EXPR_NEST_MAX
#define Posix_ProcEnv_JOB_CONTROL _SC_JOB_CONTROL
#define Posix_ProcEnv_LINE_MAX _SC_LINE_MAX
#define Posix_ProcEnv_NGROUPS_MAX _SC_NGROUPS_MAX
#define Posix_ProcEnv_OPEN_MAX _SC_OPEN_MAX
#define Posix_ProcEnv_RE_DUP_MAX _SC_RE_DUP_MAX
#define Posix_ProcEnv_SAVED_IDS _SC_SAVED_IDS
#define Posix_ProcEnv_STREAM_MAX _SC_STREAM_MAX
#define Posix_ProcEnv_TZNAME_MAX _SC_TZNAME_MAX
#define Posix_ProcEnv_VERSION _SC_VERSION

enum {
        Posix_ProcEnv_numgroups = 100,
};

Pid Posix_ProcEnv_getpid ();
Pid Posix_ProcEnv_getppid ();
Uid Posix_ProcEnv_getuid ();
Uid Posix_ProcEnv_geteuid ();
Gid Posix_ProcEnv_getgid ();
Gid Posix_ProcEnv_getegid ();
Int Posix_ProcEnv_setenv (NullString s, NullString v);
Int Posix_ProcEnv_setuid (Uid u);
Int Posix_ProcEnv_setgid (Gid g);
Int Posix_ProcEnv_getgroups (Pointer groups);
Cstring Posix_ProcEnv_getlogin ();
Pid Posix_ProcEnv_getpgrp ();
Pid Posix_ProcEnv_setsid ();
Int Posix_ProcEnv_setpgid (Pid p, Gid g);

Int Posix_ProcEnv_Uname_uname ();
Cstring Posix_ProcEnv_Uname_sysname ();
Cstring Posix_ProcEnv_Uname_nodename ();
Cstring Posix_ProcEnv_Uname_release ();
Cstring Posix_ProcEnv_Uname_version ();
Cstring Posix_ProcEnv_Uname_machine ();

Int Posix_ProcEnv_Tms_utime ();
Int Posix_ProcEnv_Tms_stime ();
Int Posix_ProcEnv_Tms_cutime ();
Int Posix_ProcEnv_Tms_cstime ();

Cstring Posix_ProcEnv_ctermid ();
Cstring Posix_ProcEnv_getenv (NullString s);
Bool Posix_ProcEnv_isatty (Fd f);
Int Posix_ProcEnv_sysconf (Int i);
Int Posix_ProcEnv_times ();
Cstring Posix_ProcEnv_ttyname (Fd f);

/* ---------------------------------- */
/*           Posix.Process            */
/* ---------------------------------- */

#define Posix_Process_wnohang WNOHANG
#define Posix_Process_W_untraced WUNTRACED

#define Posix_Signal_abrt SIGABRT
#define Posix_Signal_alrm SIGALRM
#define Posix_Signal_bus SIGBUS
#define Posix_Signal_chld SIGCHLD
#define Posix_Signal_cont SIGCONT
#define Posix_Signal_fpe SIGFPE
#define Posix_Signal_hup SIGHUP
#define Posix_Signal_ill SIGILL
#define Posix_Signal_int SIGINT
#define Posix_Signal_kill SIGKILL
#define Posix_Signal_pipe SIGPIPE
#define Posix_Signal_prof SIGPROF
#define Posix_Signal_quit SIGQUIT
#define Posix_Signal_segv SIGSEGV
#define Posix_Signal_stop SIGSTOP
#define Posix_Signal_term SIGTERM
#define Posix_Signal_tstp SIGTSTP
#define Posix_Signal_ttin SIGTTIN
#define Posix_Signal_ttou SIGTTOU
#define Posix_Signal_usr1 SIGUSR1
#define Posix_Signal_usr2 SIGUSR2
#define Posix_Signal_vtalrm SIGVTALRM

Int Posix_Process_alarm (Int i);
Int Posix_Process_exece (NullString path, Pointer args, Pointer env);
Int Posix_Process_execp (NullString file, Pointer args);
void Posix_Process_exit (Int i);
Pid Posix_Process_fork ();
Int Posix_Process_kill (Pid p, Signal s);
Int Posix_Process_pause ();
Int Posix_Process_sleep (Int i);
Pid Posix_Process_waitpid (Pid p, Pointer s, Int i);
Bool Posix_Process_ifExited (Status s);
Int Posix_Process_exitStatus (Status s);
Bool Posix_Process_ifSignaled (Status s);
Signal Posix_Process_termSig (Status s);
Bool Posix_Process_ifStopped (Status s);
Signal Posix_Process_stopSig (Status s);

/* ---------------------------------- */
/*            Posix.Signal            */
/* ---------------------------------- */

#define Posix_Signal_block SIG_BLOCK
#if (defined (NSIG))
#define Posix_Signal_numSignals NSIG
#elif (defined (_NSIG))
#define Posix_Signal_numSignals _NSIG
#else
#error Posix_Signal_numSignals not defined
#endif
#define Posix_Signal_setmask SIG_SETMASK
#define Posix_Signal_unblock SIG_UNBLOCK
Int Posix_Signal_default (Int signum);
Int Posix_Signal_handle (Int signum);
Int Posix_Signal_ignore (Int signum);
Int Posix_Signal_isDefault (Int signum, Bool *isDef);
Bool Posix_Signal_isPending (Int signum);
Int Posix_Signal_sigaddset (Int signum);
Int Posix_Signal_sigdelset (Int signum);
Int Posix_Signal_sigemptyset ();
Int Posix_Signal_sigfillset ();
Int Posix_Signal_sigprocmask (Int how);
Int Posix_Signal_sigsuspend ();

/* ---------------------------------- */
/*            Posix.SysDB             */
/* ---------------------------------- */

Cstring Posix_SysDB_Passwd_name ();
Uid Posix_SysDB_Passwd_uid ();
Gid Posix_SysDB_Passwd_gid ();
Cstring Posix_SysDB_Passwd_dir ();
Cstring Posix_SysDB_Passwd_shell ();
Bool Posix_SysDB_getpwnam (Pointer p);
Bool Posix_SysDB_getpwuid (Uid u);
Bool Posix_SysDB_getgrgid (Gid g);
Bool Posix_SysDB_getgrnam (NullString s);
Cstring Posix_SysDB_Group_name ();
Gid Posix_SysDB_Group_gid ();
CstringArray Posix_SysDB_Group_mem ();

/* ---------------------------------- */
/*             Posix.TTY              */
/* ---------------------------------- */

#define Posix_TTY_b0 B0
#define Posix_TTY_b110 B110
#define Posix_TTY_b1200 B1200
#define Posix_TTY_b134 B134
#define Posix_TTY_b150 B150
#define Posix_TTY_b1800 B1800
#define Posix_TTY_b19200 B19200
#define Posix_TTY_b200 B200
#define Posix_TTY_b2400 B2400
#define Posix_TTY_b300 B300
#define Posix_TTY_b38400 B38400
#define Posix_TTY_b4800 B4800
#define Posix_TTY_b50 B50
#define Posix_TTY_b600 B600
#define Posix_TTY_b75 B75
#define Posix_TTY_b9600 B9600
#define Posix_TTY_V_eof VEOF
#define Posix_TTY_V_eol VEOL
#define Posix_TTY_V_erase VERASE
#define Posix_TTY_V_intr VINTR
#define Posix_TTY_V_kill VKILL
#define Posix_TTY_V_min VMIN
#define Posix_TTY_V_nccs NCCS
#define Posix_TTY_V_quit VQUIT
#define Posix_TTY_V_start VSTART
#define Posix_TTY_V_stop VSTOP
#define Posix_TTY_V_susp VSUSP
#define Posix_TTY_V_time VTIME
#define Posix_TTY_I_brkint BRKINT
#define Posix_TTY_I_icrnl ICRNL
#define Posix_TTY_I_ignbrk IGNBRK
#define Posix_TTY_I_igncr IGNCR
#define Posix_TTY_I_ignpar IGNPAR
#define Posix_TTY_I_inlcr INLCR
#define Posix_TTY_I_inpck INPCK
#define Posix_TTY_I_istrip ISTRIP
#define Posix_TTY_I_ixoff IXOFF
#define Posix_TTY_I_ixon IXON
#define Posix_TTY_I_parmrk PARMRK
#define Posix_TTY_O_opost OPOST
#define Posix_TTY_C_clocal CLOCAL
#define Posix_TTY_C_cread CREAD
#define Posix_TTY_C_cs5 CS5
#define Posix_TTY_C_cs6 CS6
#define Posix_TTY_C_cs7 CS7
#define Posix_TTY_C_cs8 CS8
#define Posix_TTY_C_csize CSIZE
#define Posix_TTY_C_cstopb CSTOPB
#define Posix_TTY_C_hupcl HUPCL
#define Posix_TTY_C_parenb PARENB
#define Posix_TTY_C_parodd PARODD
#define Posix_TTY_L_echo ECHO
#define Posix_TTY_L_echoe ECHOE
#define Posix_TTY_L_echok ECHOK
#define Posix_TTY_L_echonl ECHONL
#define Posix_TTY_L_icanon ICANON
#define Posix_TTY_L_iexten IEXTEN
#define Posix_TTY_L_isig ISIG
#define Posix_TTY_L_noflsh NOFLSH
#define Posix_TTY_L_tostop TOSTOP
#define Posix_TTY_TC_sadrain TCSADRAIN
#define Posix_TTY_TC_saflush TCSAFLUSH
#define Posix_TTY_TC_sanow TCSANOW
#define Posix_TTY_TC_ion TCION
#define Posix_TTY_TC_ioff TCIOFF
#define Posix_TTY_TC_ooff TCOOFF
#define Posix_TTY_TC_oon TCOON
#define Posix_TTY_TC_iflush TCIFLUSH
#define Posix_TTY_TC_ioflush TCIOFLUSH
#define Posix_TTY_TC_oflush TCOFLUSH

Flag Posix_TTY_Termios_iflag ();
Flag Posix_TTY_Termios_oflag ();
Flag Posix_TTY_Termios_cflag ();
Flag Posix_TTY_Termios_lflag ();
Cstring Posix_TTY_Termios_cc ();
Speed Posix_TTY_Termios_cfgetospeed ();
Speed Posix_TTY_Termios_cfgetispeed ();
void Posix_TTY_Termios_setiflag (Flag f);
void Posix_TTY_Termios_setoflag (Flag f);
void Posix_TTY_Termios_setcflag (Flag f);
void Posix_TTY_Termios_setlflag (Flag f);
Int Posix_TTY_Termios_setospeed (Speed s);
Int Posix_TTY_Termios_setispeed (Speed s);
Int Posix_TTY_getattr (Fd f);
Int Posix_TTY_setattr (Fd f, Int i);
Int Posix_TTY_sendbreak (Fd f, Int i);
Int Posix_TTY_drain (Fd f);
Int Posix_TTY_flush (Fd f, Int i);
Int Posix_TTY_flow (Fd f, Int i);
Int Posix_TTY_getpgrp (Fd f);
Int Posix_TTY_setpgrp (Fd f, Pid p);

/* ------------------------------------------------- */
/*                      Ptrace                       */
/* ------------------------------------------------- */

Int Ptrace_ptrace2 (Int request, Int pid);
/* data is a word ref */
Int Ptrace_ptrace4 (Int request, Int pid, Word addr, Pointer data);

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

#if (defined (__MSVCRT__))
void MLton_initSockets ();
#else
static inline void MLton_initSockets () {}
#endif

#define NetHostDB_inAddrLen sizeof(struct in_addr)
#define NetHostDB_INADDR_ANY INADDR_ANY
#define Socket_sockAddrLenMax max(sizeof(struct sockaddr), \
                              max(sizeof(struct sockaddr_un), \
                              max(sizeof(struct sockaddr_in), \
                                  sizeof(struct sockaddr_in6))))
#define Socket_AF_UNIX PF_UNIX
#define Socket_AF_INET PF_INET
#define Socket_AF_INET6 PF_INET6
#define Socket_AF_UNSPEC PF_UNSPEC
#define Socket_SOCK_STREAM SOCK_STREAM
#define Socket_SOCK_DGRAM SOCK_DGRAM
#define Socket_Ctl_SOL_SOCKET SOL_SOCKET
#define Socket_Ctl_SO_DEBUG SO_DEBUG
#define Socket_Ctl_SO_REUSEADDR SO_REUSEADDR
#define Socket_Ctl_SO_KEEPALIVE SO_KEEPALIVE
#define Socket_Ctl_SO_DONTROUTE SO_DONTROUTE
#define Socket_Ctl_SO_LINGER SO_LINGER
#define Socket_Ctl_SO_BROADCAST SO_BROADCAST
#define Socket_Ctl_SO_OOBINLINE SO_OOBINLINE
#define Socket_Ctl_SO_SNDBUF SO_SNDBUF
#define Socket_Ctl_SO_RCVBUF SO_RCVBUF
#define Socket_Ctl_SO_TYPE SO_TYPE
#define Socket_Ctl_SO_ERROR SO_ERROR
#define Socket_Ctl_FIONBIO FIONBIO
#define Socket_Ctl_FIONREAD FIONREAD
#define Socket_Ctl_SIOCATMARK SIOCATMARK
#define Socket_SHUT_RD SHUT_RD
#define Socket_SHUT_WR SHUT_WR
#define Socket_SHUT_RDWR SHUT_RDWR
#define Socket_MSG_DONTROUTE MSG_DONTROUTE
#define Socket_MSG_DONTWAIT MSG_DONTWAIT
#define Socket_MSG_OOB MSG_OOB
#define Socket_MSG_PEEK MSG_PEEK
#define Socket_INetSock_TCP_SOL_TCP IPPROTO_TCP
#define Socket_INetSock_TCP_SO_NODELAY TCP_NODELAY

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
/*                      Windows                      */
/* ------------------------------------------------- */

Int Windows_terminate (Pid p, Int s);

/* ------------------------------------------------- */
/*                       Word8                       */
/* ------------------------------------------------- */

Char Word8_arshiftAsm (Char w, Word s);

/* ------------------------------------------------- */
/*                      Word32                       */
/* ------------------------------------------------- */

Word Word32_arshiftAsm (Word w, Word s);

#endif /* _PLATFORM_H_ */
