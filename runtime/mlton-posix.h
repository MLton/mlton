/* Copyright (C) 1999-2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 */
#ifndef _MLTON_POSIX_H
#define _MLTON_POSIX_H

#include "mlton-basis.h"

typedef Int Fd;
typedef Int Pid;
typedef Int Signal;
typedef Int Size;
typedef Int Speed;
typedef Int Ssize;
typedef Int Status;
typedef Int Syserror;
typedef Word Dirstream;
typedef Word Flag;
typedef Word Gid;
typedef Word Mode;
typedef Word Uid;

/* ------------------------------------------------- */
/*                       Error                       */
/* ------------------------------------------------- */

void Posix_Error_clearErrno ();
Int Posix_Error_gettErrno ();
Cstring Posix_Error_strerror (Syserror n);

/* ------------------------------------------------- */
/*                      FileSys                      */
/* ------------------------------------------------- */


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

/* ------------------------------------------------- */
/*                        IO                         */
/* ------------------------------------------------- */

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
Ssize Posix_IO_write (Fd fd, Pointer b, Int i, Size s);

/* ------------------------------------------------- */
/*                      ProcEnv                      */
/* ------------------------------------------------- */

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
extern CstringArray Posix_ProcEnv_environ;
Cstring Posix_ProcEnv_getenv (NullString s);
Bool Posix_ProcEnv_isatty (Fd f);
Int Posix_ProcEnv_sysconf (Int i);
Int Posix_ProcEnv_times ();
Cstring Posix_ProcEnv_ttyname (Fd f);

/* ------------------------------------------------- */
/*                      Process                      */
/* ------------------------------------------------- */

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

/* ------------------------------------------------- */
/*                      Signal                       */
/* ------------------------------------------------- */

Int Posix_Signal_default (Int signum);
Int Posix_Signal_handle (Int signum);
Int Posix_Signal_ignore (Int signum);
Int Posix_Signal_isDefault (Int signum, Bool *isDef);
Bool Posix_Signal_isPending (Int signum);
Int Posix_Signal_sigaddset (Int signum);
Int Posix_Signal_sigdelset (Int signum);
Int Posix_Signal_sigemptyset ();
Int Posix_Signal_sigfillset ();
Int Posix_Signal_sigprocmask ();
Int Posix_Signal_sigsuspend ();

/* ------------------------------------------------- */
/*                       SysDB                       */
/* ------------------------------------------------- */

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

/* ------------------------------------------------- */
/*                        TTY                        */
/* ------------------------------------------------- */

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

#endif /* #ifndef _MLTON_POSIX_H */
