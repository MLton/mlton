/* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 */

#ifndef _MLTON_PLATFORM_H_
#define _MLTON_PLATFORM_H_

#include "cenv.h"
#include "util.h"
#include "gc.h"

#if (defined (__APPLE_CC__))
#define __Darwin__
#endif

#if (defined (__CYGWIN__))
#include "platform/cygwin.h"
#elif (defined (__Darwin__))
#include "platform/darwin.h"
#elif (defined (__FreeBSD__))
#include "platform/freebsd.h"
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

#ifndef SPAWN_MODE
#define SPAWN_MODE 0
#endif

#include "types.h"
#include "basis-ffi.h"

/* ---------------------------------------------------------------- */
/*                        Runtime Init/Exit                         */
/* ---------------------------------------------------------------- */

void MLton_init (int argc, char **argv, GC_state s);
void MLton_exit (GC_state s, Int status) __attribute__ ((noreturn));

/* ---------------------------------------------------------------- */
/*                        Utility libraries                         */
/* ---------------------------------------------------------------- */

int mkdir2 (const char *pathname, mode_t mode);

/* ---------------------------------------------------------------- */
/*                        Garbage Collector                         */
/* ---------------------------------------------------------------- */

/* ------------------------------------------------- */
/*                Virtual Memory                     */
/* ------------------------------------------------- */

/* GC_displayMem displays the virtual memory mapping to stdout.  
 * It is used to diagnose memory problems. 
 */
void GC_displayMem (void);

void *GC_mmapAnon (void *start, size_t length);
void *GC_mmapAnon_safe (void *start, size_t length);
void *GC_mmapAnon_safe_protect (void *start, size_t length, 
                                size_t dead_low, size_t dead_high);
void *GC_mremap (void *start, size_t oldLength, size_t newLength);
void GC_release (void *base, size_t length);
void GC_decommit (void *base, size_t length);

size_t GC_pageSize (void);
size_t GC_totalRam (void);
size_t GC_availRam (void);

void GC_setCygwinUseMmap (bool b);

/* ------------------------------------------------- */
/*                Text Segment                       */
/* ------------------------------------------------- */

void *GC_getTextEnd (void);
void *GC_getTextStart (void);

/* ------------------------------------------------- */
/*                SigProf Handler                    */
/* ------------------------------------------------- */

void GC_setSigProfHandler (struct sigaction *sa);

/* ---------------------------------------------------------------- */
/*                         MLton libraries                          */
/* ---------------------------------------------------------------- */

/* ------------------------------------------------- */
/*                      IntInf                       */
/* ------------------------------------------------- */

/* All of these routines modify the frontier in gcState.  They assume that 
 * there are bytes bytes free, and allocate an array to store the result
 * at the current frontier position.
 * Immediately after the bytesArg, they take a labelIndex arg.  This is an index
 * into the array used for allocation profiling, and the appropriate element
 * is incremented by the amount that the function moves the frontier.
 */
Pointer IntInf_add (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_andb (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_arshift (Pointer arg, Word shift, size_t bytes);
Pointer IntInf_gcd (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_lshift (Pointer arg, Word shift, size_t bytes);
Pointer IntInf_mul (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_neg (Pointer arg, size_t bytes);
Pointer IntInf_notb (Pointer arg, size_t bytes);
Pointer IntInf_orb (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_quot (Pointer num, Pointer den, size_t bytes);
Pointer IntInf_rem (Pointer num, Pointer den, size_t bytes);
Pointer IntInf_sub (Pointer lhs, Pointer rhs, size_t bytes);
Pointer IntInf_toString (Pointer arg, int base, size_t bytes);
Pointer IntInf_xorb (Pointer lhs, Pointer rhs, size_t bytes);

Word IntInf_smallMul (Word lhs, Word rhs, Pointer carry);
Int IntInf_compare (Pointer lhs, Pointer rhs);
Bool IntInf_equal (Pointer lhs, Pointer rhs);

/* ------------------------------------------------- */
/*                       MLton                       */
/* ------------------------------------------------- */

void MLton_allocTooLarge (void) __attribute__ ((noreturn));
/* print a bug message and exit (2) */
void MLton_bug (Pointer msg) __attribute__ ((noreturn));

/* ---------------------------------- */
/*           MLton.Platform           */
/* ---------------------------------- */

#define MLton_Platform_Arch_bigendian isBigEndian()

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
/*           MLton.Process            */
/* ---------------------------------- */

Pid MLton_Process_cwait (Pid p, Pointer s);
Int MLton_Process_spawne (Pointer p, Pointer a, Pointer e);
Int MLton_Process_spawnp (Pointer p, Pointer a);

/* ------------------------------------------------- */
/*                     PackReal                      */
/* ------------------------------------------------- */

Real32 PackReal32_subVec (Pointer v, Int offset);
Real32 PackReal32_subVecRev (Pointer v, Int offset);
Real64 PackReal64_subVec (Pointer v, Int offset);
Real64 PackReal64_subVecRev (Pointer v, Int offset);
void PackReal32_update (Pointer a, Int offset, Real32 r);
void PackReal32_updateRev (Pointer a, Int offset, Real32 r);
void PackReal64_update (Pointer a, Int offset, Real64 r);
void PackReal64_updateRev (Pointer a, Int offset, Real64 r);

/* ------------------------------------------------- */
/*                       Posix                       */
/* ------------------------------------------------- */

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

void Posix_FileSys_Utimbuf_setActime (Int x);
void Posix_FileSys_Utimbuf_setModTime (Int x);
Int Posix_FileSys_Utimbuf_utime (Pointer s);

Int Posix_FileSys_access (Pointer f, Word w);
Int Posix_FileSys_chdir(Pointer p);
Int Posix_FileSys_chmod (Pointer p, Mode m);
Int Posix_FileSys_chown (Pointer p, Uid u, Gid g);
Int Posix_FileSys_fchmod (Fd f, Mode m);
Int Posix_FileSys_fchown (Fd f, Uid u, Gid g);
Int Posix_FileSys_fpathconf (Fd f, Int n);
Int Posix_FileSys_ftruncate (Fd f, Position n);
Cstring Posix_FileSys_getcwd (Pointer buf, Size n);
Int Posix_FileSys_link (Pointer p1, Pointer p2);
Int Posix_FileSys_mkdir (Pointer p, Word w);
Int Posix_FileSys_mkfifo (Pointer p, Word w);
Int Posix_FileSys_open (Pointer p, Word w, Mode m);
Int Posix_FileSys_pathconf (Pointer p, Int n);
Int Posix_FileSys_readlink (Pointer p, Pointer b, Int);
Int Posix_FileSys_rename (Pointer p1, Pointer p2);
Int Posix_FileSys_rmdir (Pointer p);
Int Posix_FileSys_symlink (Pointer p1, Pointer p2);
Word Posix_FileSys_umask (Word w);
Word Posix_FileSys_unlink (Pointer p);

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
Int Posix_IO_FLock_type (void);
Int Posix_IO_FLock_whence (void);
Position Posix_IO_FLock_start (void);
Position Posix_IO_FLock_len (void);
Int Posix_IO_FLock_pid (void);
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

Pid Posix_ProcEnv_getpid (void);
Pid Posix_ProcEnv_getppid (void);
Uid Posix_ProcEnv_getuid (void);
Uid Posix_ProcEnv_geteuid (void);
Gid Posix_ProcEnv_getgid (void);
Gid Posix_ProcEnv_getegid (void);
Int Posix_ProcEnv_setenv (Pointer s, Pointer v);
Int Posix_ProcEnv_setuid (Uid u);
Int Posix_ProcEnv_setgid (Gid g);
Int Posix_ProcEnv_getgroups (Pointer groups);
Int Posix_ProcEnv_setgroups (Pointer groups);
Cstring Posix_ProcEnv_getlogin (void);
Pid Posix_ProcEnv_getpgrp (void);
Pid Posix_ProcEnv_setsid (void);
Int Posix_ProcEnv_setpgid (Pid p, Gid g);

Int Posix_ProcEnv_Uname_uname (void);
Cstring Posix_ProcEnv_Uname_sysname (void);
Cstring Posix_ProcEnv_Uname_nodename (void);
Cstring Posix_ProcEnv_Uname_release (void);
Cstring Posix_ProcEnv_Uname_version (void);
Cstring Posix_ProcEnv_Uname_machine (void);

Int Posix_ProcEnv_Tms_utime (void);
Int Posix_ProcEnv_Tms_stime (void);
Int Posix_ProcEnv_Tms_cutime (void);
Int Posix_ProcEnv_Tms_cstime (void);

Cstring Posix_ProcEnv_ctermid (void);
Cstring Posix_ProcEnv_getenv (Pointer s);
Bool Posix_ProcEnv_isatty (Fd f);
Int Posix_ProcEnv_sysconf (Int i);
Int Posix_ProcEnv_times (void);
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
Int Posix_Process_exece (Pointer path, Pointer args, Pointer env);
Int Posix_Process_execp (Pointer file, Pointer args);
void Posix_Process_exit (Int i) __attribute__ ((noreturn));
Pid Posix_Process_fork (void);
Int Posix_Process_kill (Pid p, Signal s);
Int Posix_Process_nanosleep (Pointer sec, Pointer nsec);
Int Posix_Process_pause (void);
Int Posix_Process_sleep (Int i);
int Posix_Process_system (const char* cmd);
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
bool Posix_Signal_isGCPending (void);
Bool Posix_Signal_isPending (Int signum);
Int Posix_Signal_handle (Int signum);
void Posix_Signal_handleGC (void);
Int Posix_Signal_ignore (Int signum);
Int Posix_Signal_isDefault (Int signum, Bool *isDef);
void Posix_Signal_resetPending (void);

Int Posix_Signal_sigaddset (Int signum);
Int Posix_Signal_sigdelset (Int signum);
Int Posix_Signal_sigemptyset (void);
Int Posix_Signal_sigfillset (void);
Int Posix_Signal_sigismember (Int signum);
Int Posix_Signal_sigprocmask (Int how);
void Posix_Signal_suspend (void);

/* ---------------------------------- */
/*            Posix.SysDB             */
/* ---------------------------------- */

Cstring Posix_SysDB_Passwd_name (void);
Uid Posix_SysDB_Passwd_uid (void);
Gid Posix_SysDB_Passwd_gid (void);
Cstring Posix_SysDB_Passwd_dir (void);
Cstring Posix_SysDB_Passwd_shell (void);
Bool Posix_SysDB_getpwnam (Pointer p);
Bool Posix_SysDB_getpwuid (Uid u);
Bool Posix_SysDB_getgrgid (Gid g);
Bool Posix_SysDB_getgrnam (Pointer s);
Cstring Posix_SysDB_Group_name (void);
Gid Posix_SysDB_Group_gid (void);
CstringArray Posix_SysDB_Group_mem (void);

/* ------------------------------------------------- */
/*                      Ptrace                       */
/* ------------------------------------------------- */

Int Ptrace_ptrace2 (Int request, Int pid);
/* data is a word ref */
Int Ptrace_ptrace4 (Int request, Int pid, Word addr, Pointer data);

/* ------------------------------------------------- */
/*                       Real                        */
/* ------------------------------------------------- */

Real64 Real64_modf (Real64 x, Real64 *exp);
Real32 Real32_modf (Real32 x, Real32 *exp);
Real64 Real64_frexp (Real64 x, Int *exp);
Cstring Real64_gdtoa (double d, int mode, int ndig, int *decpt);
Cstring Real32_gdtoa (float f, int mode, int ndig, int *decpt);
Int Real32_class (Real32 f);
Int Real64_class (Real64 d);
Real32 Real32_strto (Pointer s);
Real64 Real64_strto (Pointer s);
Real64 Real64_nextAfter (Real64 x1, Real64 x2);
Int Real32_signBit (Real32 f);
Int Real64_signBit (Real64 d);
#define ternary(size, name)                                     \
        Real##size Real##size##_mul##name                       \
                (Real##size r1, Real##size r2, Real##size r3);
ternary(32, add)
ternary(64, add)
ternary(32, sub)
ternary(64, sub)
#undef ternary

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

#if (defined (__MSVCRT__))
void MLton_initSockets (void);
#else
static inline void MLton_initSockets (void) {}
#endif

/* ------------------------------------------------- */
/*                      Windows                      */
/* ------------------------------------------------- */

Int Windows_terminate (Pid p, Int s);

/* ------------------------------------------------- */
/*                  Word{8,16,32,64}                 */
/* ------------------------------------------------- */

#define SaddCheckOverflows(size)                                        \
        Bool WordS##size##_addCheckOverflows (WordS##size x, WordS##size y);
#define UaddCheckOverflows(size)                                        \
        Bool WordU##size##_addCheckOverflows (WordU##size x, WordU##size y);
#define SmulCheckOverflows(size)                                        \
        Bool WordS##size##_mulCheckOverflows (WordS##size x, WordS##size y);
#define negCheckOverflows(size)                                         \
        Bool Word##size##_negCheckOverflows (WordS##size x);
#define SsubCheckOverflows(size)                                        \
        Bool WordS##size##_subCheckOverflows (WordS##size x, WordS##size y);
#define all(size)                                               \
        SaddCheckOverflows (size)                               \
        UaddCheckOverflows (size)                               \
        SmulCheckOverflows (size)                               \
        negCheckOverflows (size)                                \
        SsubCheckOverflows (size)
all (8)
all (16)
all (32)
all (64)
#undef SaddCheckOverflows
#undef UaddCheckOverflows
#undef SmulCheckOverflows
#undef negCheckOverflows
#undef SsubCheckOverflows
#undef all

/* ------------------------------------------------- */
/*                    Word8 Array                    */
/* ------------------------------------------------- */

Word32 Word8Array_subWord32Rev (Pointer v, Int offset);
void Word8Array_updateWord32Rev (Pointer a, Int offset, Word32 w);

/* ------------------------------------------------- */
/*                    Word8 Vector                   */
/* ------------------------------------------------- */

Word32 Word8Vector_subWord32Rev (Pointer v, Int offset);

#endif /* _MLTON_PLATFORM_H_ */
