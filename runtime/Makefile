HOST = self
HOSTTYPE = $(shell ../bin/hosttype)
ifeq ($(HOST), self)
AR = ar rc
HOSTFLAGS =
else
AR = $(HOST)-ar rc
HOSTFLAGS = -b $(HOST)
endif
CC =		gcc -Wall -I. -mcpu=pentiumpro -malign-loops=2 -malign-jumps=2 -malign-functions=5 -fomit-frame-pointer $(HOSTFLAGS)
# Can't use more optimization than -O1 because gcc doesn't correctly compile
#  Real_class in basis/Real.c
CFLAGS =	-O1 -DNODEBUG

OBJS =						\
	basis/Array/numElements.o		\
	basis/C.o				\
	basis/CommandLine.o			\
	basis/Date.o				\
	basis/Debug.o				\
	basis/GC/setMessages.o			\
	basis/GC/setSummary.o			\
	basis/IEEEReal.o			\
	basis/IntInf.o				\
	basis/Int/addOverflow.o			\
	basis/Int/mulOverflow.o			\
	basis/Int/negOverflow.o			\
	basis/Int/quot.o			\
	basis/Int/rem.o				\
	basis/Int/subOverflow.o			\
	basis/Itimer/set.o			\
	basis/MLton/allocTooLarge.o		\
	basis/MLton/bug.o			\
	basis/MLton/errno.o			\
	basis/MLton/exit.o			\
	basis/MLton/profile.o			\
	basis/MLton/rlimit.o			\
	basis/MLton/rusage.o			\
	basis/MLton/spawne.o			\
	basis/MLton/spawnp.o			\
	basis/MLton/size.o			\
	basis/MLton/world.o			\
	basis/OS/FileSys/tmpnam.o		\
	basis/PackReal/subVec.o			\
	basis/PackReal/update.o			\
	basis/Ptrace/ptrace2.o			\
	basis/Ptrace/ptrace4.o			\
	basis/Real.o				\
	basis/Real_const.o			\
	basis/Socket/Host.o			\
	basis/Socket/accept.o			\
	basis/Socket/connect.o			\
	basis/Socket/listen.o			\
	basis/Socket/shutdown.o			\
	basis/Stdio.o				\
	basis/Thread.o				\
	basis/Time.o				\
	basis/Word32/addOverflow.o		\
	basis/Word32/arshiftAsm.o		\
	basis/Word32/mulOverflow.o		\
	basis/Word8/arshiftAsm.o		\
	Posix/Error/clearErrno.o		\
	Posix/Error/getErrno.o			\
	Posix/Error/strerror.o			\
	Posix/FileSys/Dirstream/closedir.o	\
	Posix/FileSys/Dirstream/opendir.o	\
	Posix/FileSys/Dirstream/readdir.o	\
	Posix/FileSys/Dirstream/rewinddir.o	\
	Posix/FileSys/ST/isBlk.o		\
	Posix/FileSys/ST/isChr.o		\
	Posix/FileSys/ST/isDir.o		\
	Posix/FileSys/ST/isFIFO.o		\
	Posix/FileSys/ST/isLink.o		\
	Posix/FileSys/ST/isReg.o		\
	Posix/FileSys/ST/isSock.o		\
	Posix/FileSys/Stat.o			\
	Posix/FileSys/Utimbuf.o			\
	Posix/FileSys/access.o			\
	Posix/FileSys/chdir.o			\
	Posix/FileSys/chmod.o			\
	Posix/FileSys/chown.o			\
	Posix/FileSys/fchmod.o			\
	Posix/FileSys/fchown.o			\
	Posix/FileSys/fpathconf.o		\
	Posix/FileSys/ftruncate.o		\
	Posix/FileSys/getcwd.o			\
	Posix/FileSys/link.o			\
	Posix/FileSys/mkdir.o			\
	Posix/FileSys/mkfifo.o			\
	Posix/FileSys/open.o			\
	Posix/FileSys/pathconf.o		\
	Posix/FileSys/readlink.o		\
	Posix/FileSys/rename.o			\
	Posix/FileSys/rmdir.o			\
	Posix/FileSys/symlink.o			\
	Posix/FileSys/umask.o			\
	Posix/FileSys/unlink.o			\
	Posix/IO/FLock.o			\
	Posix/IO/close.o			\
	Posix/IO/dup.o				\
	Posix/IO/dup2.o				\
	Posix/IO/fcntl2.o			\
	Posix/IO/fcntl3.o			\
	Posix/IO/fsync.o			\
	Posix/IO/lseek.o			\
	Posix/IO/pipe.o				\
	Posix/IO/read.o				\
	Posix/IO/write.o			\
	Posix/ProcEnv/Tms.o			\
	Posix/ProcEnv/Uname.o			\
	Posix/ProcEnv/ctermid.o			\
	Posix/ProcEnv/environ.o			\
	Posix/ProcEnv/getenv.o			\
	Posix/ProcEnv/getegid.o			\
	Posix/ProcEnv/geteuid.o			\
	Posix/ProcEnv/getgid.o			\
	Posix/ProcEnv/getgroups.o		\
	Posix/ProcEnv/getlogin.o		\
	Posix/ProcEnv/getpgrp.o			\
	Posix/ProcEnv/getpid.o			\
	Posix/ProcEnv/getppid.o			\
	Posix/ProcEnv/getuid.o			\
	Posix/ProcEnv/isatty.o			\
	Posix/ProcEnv/setenv.o			\
	Posix/ProcEnv/setgid.o			\
	Posix/ProcEnv/setpgid.o			\
	Posix/ProcEnv/setsid.o			\
	Posix/ProcEnv/setuid.o			\
	Posix/ProcEnv/sysconf.o			\
	Posix/ProcEnv/ttyname.o			\
	Posix/Process/alarm.o			\
	Posix/Process/exece.o			\
	Posix/Process/execp.o			\
	Posix/Process/exit.o			\
	Posix/Process/exitStatus.o		\
	Posix/Process/fork.o			\
	Posix/Process/ifExited.o		\
	Posix/Process/ifSignaled.o		\
	Posix/Process/ifStopped.o		\
	Posix/Process/kill.o			\
	Posix/Process/pause.o			\
	Posix/Process/sleep.o			\
	Posix/Process/stopSig.o			\
	Posix/Process/termSig.o			\
	Posix/Process/waitpid.o			\
	Posix/Signal/Signal.o			\
	Posix/Signal/isPending.o		\
	Posix/SysDB/Group.o			\
	Posix/SysDB/Passwd.o			\
	Posix/TTY/Termios.o			\
	Posix/TTY/drain.o			\
	Posix/TTY/flow.o			\
	Posix/TTY/flush.o			\
	Posix/TTY/getpgrp.o			\
	Posix/TTY/sendbreak.o			\
	Posix/TTY/setpgrp.o			\
	bcopy.o					\
	gc.o					\
	libmlton.o				\
	my-lib.o

DEBUG_OBJS =					\
	basis/Array/numElements-gdb.o		\
	basis/C-gdb.o				\
	basis/CommandLine-gdb.o			\
	basis/Date-gdb.o			\
	basis/Debug-gdb.o			\
	basis/GC/setMessages-gdb.o		\
	basis/GC/setSummary-gdb.o		\
	basis/IEEEReal-gdb.o			\
	basis/IntInf-gdb.o			\
	basis/Int/addOverflow-gdb.o		\
	basis/Int/mulOverflow-gdb.o		\
	basis/Int/negOverflow-gdb.o		\
	basis/Int/quot-gdb.o			\
	basis/Int/rem-gdb.o			\
	basis/Int/subOverflow-gdb.o		\
	basis/Itimer/set-gdb.o			\
	basis/MLton/allocTooLarge-gdb.o		\
	basis/MLton/bug-gdb.o			\
	basis/MLton/errno-gdb.o			\
	basis/MLton/exit-gdb.o			\
	basis/MLton/profile-gdb.o		\
	basis/MLton/rlimit-gdb.o		\
	basis/MLton/rusage-gdb.o		\
	basis/MLton/spawne-gdb.o		\
	basis/MLton/spawnp-gdb.o		\
	basis/MLton/size-gdb.o			\
	basis/MLton/world-gdb.o			\
	basis/OS/FileSys/tmpnam-gdb.o		\
	basis/PackReal/subVec-gdb.o		\
	basis/PackReal/update-gdb.o		\
	basis/Ptrace/ptrace2-gdb.o		\
	basis/Ptrace/ptrace4-gdb.o		\
	basis/Real-gdb.o			\
	basis/Real_const-gdb.o			\
	basis/Socket/Host-gdb.o			\
	basis/Socket/accept-gdb.o		\
	basis/Socket/connect-gdb.o		\
	basis/Socket/listen-gdb.o		\
	basis/Socket/shutdown-gdb.o		\
	basis/Stdio-gdb.o			\
	basis/Thread-gdb.o			\
	basis/Time-gdb.o			\
	basis/Word32/addOverflow-gdb.o		\
	basis/Word32/arshiftAsm-gdb.o		\
	basis/Word32/mulOverflow-gdb.o		\
	basis/Word8/arshiftAsm-gdb.o		\
	Posix/Error/clearErrno-gdb.o		\
	Posix/Error/getErrno-gdb.o		\
	Posix/Error/strerror-gdb.o		\
	Posix/FileSys/Dirstream/closedir-gdb.o	\
	Posix/FileSys/Dirstream/opendir-gdb.o	\
	Posix/FileSys/Dirstream/readdir-gdb.o	\
	Posix/FileSys/Dirstream/rewinddir-gdb.o	\
	Posix/FileSys/ST/isBlk-gdb.o		\
	Posix/FileSys/ST/isChr-gdb.o		\
	Posix/FileSys/ST/isDir-gdb.o		\
	Posix/FileSys/ST/isFIFO-gdb.o		\
	Posix/FileSys/ST/isLink-gdb.o		\
	Posix/FileSys/ST/isReg-gdb.o		\
	Posix/FileSys/ST/isSock-gdb.o		\
	Posix/FileSys/Stat-gdb.o		\
	Posix/FileSys/Utimbuf-gdb.o		\
	Posix/FileSys/access-gdb.o		\
	Posix/FileSys/chdir-gdb.o		\
	Posix/FileSys/chmod-gdb.o		\
	Posix/FileSys/chown-gdb.o		\
	Posix/FileSys/fchmod-gdb.o		\
	Posix/FileSys/fchown-gdb.o		\
	Posix/FileSys/fpathconf-gdb.o		\
	Posix/FileSys/ftruncate-gdb.o		\
	Posix/FileSys/getcwd-gdb.o		\
	Posix/FileSys/link-gdb.o		\
	Posix/FileSys/mkdir-gdb.o		\
	Posix/FileSys/mkfifo-gdb.o		\
	Posix/FileSys/open-gdb.o		\
	Posix/FileSys/pathconf-gdb.o		\
	Posix/FileSys/readlink-gdb.o		\
	Posix/FileSys/rename-gdb.o		\
	Posix/FileSys/rmdir-gdb.o		\
	Posix/FileSys/symlink-gdb.o		\
	Posix/FileSys/umask-gdb.o		\
	Posix/FileSys/unlink-gdb.o		\
	Posix/IO/FLock-gdb.o			\
	Posix/IO/close-gdb.o			\
	Posix/IO/dup-gdb.o			\
	Posix/IO/dup2-gdb.o			\
	Posix/IO/fcntl2-gdb.o			\
	Posix/IO/fcntl3-gdb.o			\
	Posix/IO/fsync-gdb.o			\
	Posix/IO/lseek-gdb.o			\
	Posix/IO/pipe-gdb.o			\
	Posix/IO/read-gdb.o			\
	Posix/IO/write-gdb.o			\
	Posix/ProcEnv/Tms-gdb.o			\
	Posix/ProcEnv/Uname-gdb.o		\
	Posix/ProcEnv/ctermid-gdb.o		\
	Posix/ProcEnv/environ-gdb.o		\
	Posix/ProcEnv/getenv-gdb.o		\
	Posix/ProcEnv/getegid-gdb.o		\
	Posix/ProcEnv/geteuid-gdb.o		\
	Posix/ProcEnv/getgid-gdb.o		\
	Posix/ProcEnv/getgroups-gdb.o		\
	Posix/ProcEnv/getlogin-gdb.o		\
	Posix/ProcEnv/getpgrp-gdb.o		\
	Posix/ProcEnv/getpid-gdb.o		\
	Posix/ProcEnv/getppid-gdb.o		\
	Posix/ProcEnv/getuid-gdb.o		\
	Posix/ProcEnv/isatty-gdb.o		\
	Posix/ProcEnv/setenv-gdb.o		\
	Posix/ProcEnv/setgid-gdb.o		\
	Posix/ProcEnv/setpgid-gdb.o		\
	Posix/ProcEnv/setsid-gdb.o		\
	Posix/ProcEnv/setuid-gdb.o		\
	Posix/ProcEnv/sysconf-gdb.o		\
	Posix/ProcEnv/ttyname-gdb.o		\
	Posix/Process/alarm-gdb.o		\
	Posix/Process/exece-gdb.o		\
	Posix/Process/execp-gdb.o		\
	Posix/Process/exit-gdb.o		\
	Posix/Process/exitStatus-gdb.o		\
	Posix/Process/fork-gdb.o		\
	Posix/Process/ifExited-gdb.o		\
	Posix/Process/ifSignaled-gdb.o		\
	Posix/Process/ifStopped-gdb.o		\
	Posix/Process/kill-gdb.o		\
	Posix/Process/pause-gdb.o		\
	Posix/Process/sleep-gdb.o		\
	Posix/Process/stopSig-gdb.o		\
	Posix/Process/termSig-gdb.o		\
	Posix/Process/waitpid-gdb.o		\
	Posix/Signal/Signal-gdb.o		\
	Posix/Signal/isPending-gdb.o		\
	Posix/SysDB/Group-gdb.o			\
	Posix/SysDB/Passwd-gdb.o		\
	Posix/TTY/Termios-gdb.o			\
	Posix/TTY/drain-gdb.o			\
	Posix/TTY/flow-gdb.o			\
	Posix/TTY/flush-gdb.o			\
	Posix/TTY/getpgrp-gdb.o			\
	Posix/TTY/sendbreak-gdb.o		\
	Posix/TTY/setpgrp-gdb.o			\
	bcopy.o					\
	gc-gdb.o				\
	libmlton-gdb.o				\
	my-lib-gdb.o

%-gdb.o: %.c
	$(CC) -g -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%-gdb.o: %.S
	$(CC) -g -c -o $@ $<

%.o: %.S
	$(CC) $(CFLAGS) -c -o $@ $<

all: libmlton.a libmlton-gdb.a

libmlton.a: $(OBJS) 
	$(AR) libmlton.a $(OBJS)

libmlton-gdb.a: $(DEBUG_OBJS)
	$(AR) libmlton-gdb.a $(DEBUG_OBJS)

.PHONY: clean
clean:
	../bin/clean

.PHONY: depend
depend:
	makedepend -f- -- $(CFLAGS) -- $(SRCS)
