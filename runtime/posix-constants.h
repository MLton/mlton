#ifndef _POSIX_CONSTANTS_H_
#define _POSIX_CONSTANTS_H_

#include <errno.h>
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

#define Posix_FileSys_S_ifsock S_IFSOCK
#define Posix_FileSys_S_iflnk S_IFLNK
#define Posix_FileSys_S_ifreg S_IFREG
#define Posix_FileSys_S_ifblk S_IFBLK
#define Posix_FileSys_S_ifdir S_IFDIR
#define Posix_FileSys_S_ifchr S_IFCHR
#define Posix_FileSys_S_ififo S_IFIFO


#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

/* Cygwin/Windows distinguish between text and binary files, but Linux
 * does not.
 */
#if (defined (__linux__) || defined (__FreeBSD__))
#define O_BINARY 0
#define O_TEXT 0
#endif

#define Posix_FileSys_O_append O_APPEND
#define Posix_FileSys_O_binary O_BINARY
#define Posix_FileSys_O_creat O_CREAT
#define Posix_FileSys_O_excl O_EXCL
#define Posix_FileSys_O_noctty O_NOCTTY
#define Posix_FileSys_O_nonblock O_NONBLOCK
#if (defined (__CYGWIN__) || defined (__linux__))
#define Posix_FileSys_O_sync O_SYNC
#elif (defined (__FreeBSD__))
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

#include <unistd.h>
#define Posix_FileSys_R_OK R_OK
#define Posix_FileSys_W_OK W_OK
#define Posix_FileSys_X_OK X_OK
#define Posix_FileSys_F_OK F_OK

#include <unistd.h>
/* used by pathconf and fpathconf */
#define Posix_FileSys_LINK_MAX _PC_LINK_MAX
#define Posix_FileSys_MAX_CANON _PC_MAX_CANON
#define Posix_FileSys_MAX_INPUT _PC_MAX_INPUT
#define Posix_FileSys_NAME_MAX _PC_NAME_MAX
#define Posix_FileSys_PATH_MAX _PC_PATH_MAX
#define Posix_FileSys_PIPE_BUF _PC_PIPE_BUF
#define Posix_FileSys_CHOWN_RESTRICTED _PC_CHOWN_RESTRICTED
#define Posix_FileSys_NO_TRUNC _PC_NO_TRUNC
#define Posix_FileSys_VDISABLE _PC_VDISABLE

#include <unistd.h>
#include <fcntl.h>
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

#include <unistd.h>
#if (defined (__CYGWIN__))
#define _SC_BOGUS 0xFFFFFFFF
#define _SC_2_FORT_DEV _SC_BOGUS
#define _SC_2_FORT_RUN _SC_BOGUS
#define _SC_2_SW_DEV _SC_BOGUS
#define _SC_2_VERSION _SC_BOGUS
#define _SC_BC_BASE_MAX _SC_BOGUS
#define _SC_BC_DIM_MAX _SC_BOGUS
#define _SC_BC_SCALE_MAX _SC_BOGUS
#define _SC_BC_STRING_MAX _SC_BOGUS
#define _SC_COLL_WEIGHTS_MAX _SC_BOGUS
#define _SC_EXPR_NEST_MAX _SC_BOGUS
#define _SC_LINE_MAX _SC_BOGUS
#define _SC_RE_DUP_MAX _SC_BOGUS
#define _SC_STREAM_MAX _SC_BOGUS
#endif

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
#define Posix_ProcEnv_OPEN_MAX _SC_OPEN_MAX
#define Posix_ProcEnv_RE_DUP_MAX _SC_RE_DUP_MAX
#define Posix_ProcEnv_SAVED_IDS _SC_SAVED_IDS
#define Posix_ProcEnv_STREAM_MAX _SC_STREAM_MAX
#define Posix_ProcEnv_TZNAME_MAX _SC_TZNAME_MAX
#define Posix_ProcEnv_VERSION _SC_VERSION

enum {
	Posix_ProcEnv_numgroups = 100,
};

#include <sys/wait.h>
#define Posix_Process_wnohang WNOHANG
#define Posix_Process_W_untraced WUNTRACED

#include <signal.h>
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

#define Posix_Signal_block SIG_BLOCK
#if (defined (__CYGWIN__) || defined (__FreeBSD__))
#define Posix_Signal_numSignals NSIG
#elif (defined (__linux__))
#define Posix_Signal_numSignals _NSIG
#endif
#define Posix_Signal_setmask SIG_SETMASK
#define Posix_Signal_unblock SIG_UNBLOCK

#include <termios.h>
#include <unistd.h>

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

#endif /* #ifndef _POSIX_CONSTANTS_H_ */
