# Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
#    Jagannathan, and Stephen Weeks.
#
# MLton is released under the GNU General Public License (GPL).
# Please see the file MLton-LICENSE for license information.
#

TARGET = self
TARGET_ARCH = $(shell ../bin/host-arch)
TARGET_OS = $(shell ../bin/host-os)
GCC_VERSION = $(shell gcc -v 2>&1 | grep 'gcc version' | sed 's/.*gcc version \(.\).*/\1/')

FLAGS = -fomit-frame-pointer

ifeq ($(TARGET_ARCH), x86)
FLAGS += -mcpu=pentiumpro 
ifeq ($(GCC_VERSION), 3)
FLAGS += -falign-loops=2 -falign-jumps=2 -falign-functions=5
else
FLAGS += -malign-loops=2 -malign-jumps=2 -malign-functions=5
endif
endif
ifeq ($(TARGET_ARCH), sparc)
FLAGS += -Wa,-xarch=v8plusa -fcall-used-g5 -fcall-used-g7 -funroll-all-loops -m32 -mv8 -mcpu=ultrasparc
endif

ifeq ($(TARGET), self)
AR = ar rc
else
AR = $(TARGET)-ar rc
FLAGS += -b $(TARGET)
endif

CC = gcc -Wall -I. -D_FILE_OFFSET_BITS=64 $(FLAGS)
CFLAGS = -O2
DEBUGFLAGS = -gstabs+ -g2

OBJS =						\
	basis/Array/numElements.o		\
	basis/CommandLine.o			\
	basis/Date.o				\
	basis/Debug.o				\
	basis/GC.o				\
	basis/IEEEReal.o			\
	basis/IntInf.o				\
	basis/Int/Word.o			\
	basis/Int/Word8Array.o			\
	basis/Int/Word8Vector.o			\
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
	basis/Net/Net.o				\
	basis/Net/NetHostDB.o			\
	basis/Net/NetProtDB.o			\
	basis/Net/NetServDB.o			\
	basis/Net/Socket/INetSock.o		\
	basis/Net/Socket/Socket.o		\
	basis/OS/IO/poll.o			\
	basis/PackReal.o			\
	basis/Ptrace.o				\
	basis/Real/class.o			\
	basis/Real/frexp.o			\
	basis/Real/gdtoa.o			\
	basis/Real/modf.o			\
	basis/Real/nextAfter.o			\
	basis/Real/real.o			\
	basis/Real/signBit.o			\
	basis/Real/strto.o			\
	basis/Stdio.o				\
	basis/Thread.o				\
	basis/Time.o				\
	Posix/Error.o				\
	Posix/FileSys/Dirstream.o		\
	Posix/FileSys/ST.o			\
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
	Posix/ProcEnv/ProcEnv.o			\
	Posix/ProcEnv/Tms.o			\
	Posix/ProcEnv/Uname.o			\
	Posix/ProcEnv/environ.o			\
	Posix/ProcEnv/getenv.o			\
	Posix/ProcEnv/getgroups.o		\
	Posix/ProcEnv/getlogin.o		\
	Posix/ProcEnv/getpgrp.o			\
	Posix/ProcEnv/isatty.o			\
	Posix/ProcEnv/setenv.o			\
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
	Posix/Signal.o				\
	Posix/SysDB/Group.o			\
	Posix/SysDB/Passwd.o			\
	Posix/TTY.o				\
	gc.o					\
	platform.o				\
	platform/$(TARGET_OS).o

DEBUG_OBJS =					\
	basis/Array/numElements-gdb.o		\
	basis/CommandLine-gdb.o			\
	basis/Date-gdb.o			\
	basis/Debug-gdb.o			\
	basis/GC-gdb.o				\
	basis/IEEEReal-gdb.o			\
	basis/IntInf-gdb.o			\
	basis/Int/Word-gdb.o			\
	basis/Int/Word8Array-gdb.o		\
	basis/Int/Word8Vector-gdb.o		\
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
	basis/Net/Net-gdb.o	 		\
	basis/Net/NetHostDB-gdb.o		\
	basis/Net/NetProtDB-gdb.o		\
	basis/Net/NetServDB-gdb.o		\
	basis/Net/Socket/INetSock-gdb.o		\
	basis/Net/Socket/Socket-gdb.o		\
	basis/OS/IO/poll-gdb.o			\
	basis/PackReal-gdb.o			\
	basis/Ptrace-gdb.o			\
	basis/Real/class-gdb.o			\
	basis/Real/frexp-gdb.o			\
	basis/Real/gdtoa-gdb.o			\
	basis/Real/modf-gdb.o			\
	basis/Real/nextAfter-gdb.o		\
	basis/Real/real-gdb.o			\
	basis/Real/signBit-gdb.o		\
	basis/Real/strto-gdb.o			\
	basis/Stdio-gdb.o			\
	basis/Thread-gdb.o			\
	basis/Time-gdb.o			\
	Posix/Error-gdb.o			\
	Posix/FileSys/Dirstream-gdb.o		\
	Posix/FileSys/ST-gdb.o			\
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
	Posix/ProcEnv/ProcEnv-gdb.o		\
	Posix/ProcEnv/Tms-gdb.o			\
	Posix/ProcEnv/Uname-gdb.o		\
	Posix/ProcEnv/environ-gdb.o		\
	Posix/ProcEnv/getenv-gdb.o		\
	Posix/ProcEnv/getgroups-gdb.o		\
	Posix/ProcEnv/getlogin-gdb.o		\
	Posix/ProcEnv/getpgrp-gdb.o		\
	Posix/ProcEnv/isatty-gdb.o		\
	Posix/ProcEnv/setenv-gdb.o		\
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
	Posix/Signal-gdb.o			\
	Posix/SysDB/Group-gdb.o			\
	Posix/SysDB/Passwd-gdb.o		\
	Posix/TTY-gdb.o				\
	gc-gdb.o				\
	platform-gdb.o				\
	platform/$(TARGET_OS)-gdb.o

all:  libgdtoa.a libmlton.a libmlton-gdb.a

libgdtoa.a: gdtoa/arith.h
	cd gdtoa && $(CC) $(CFLAGS) -w -O1 -c -DINFNAN_CHECK *.c
	$(AR) libgdtoa.a gdtoa/*.o

gdtoa/arithchk.c:
	zcat gdtoa.tgz | tar xf -	
	patch -p0 <gdtoa-patch

gdtoa/arithchk.out: gdtoa/arithchk.c
	cd gdtoa && $(CC) -o arithchk.out arithchk.c

gdtoa/arith.h: gdtoa/arithchk.out
	cd gdtoa && ./arithchk.out >arith.h

libmlton.a: $(OBJS) 
	$(AR) libmlton.a $(OBJS)

libmlton-gdb.a: $(DEBUG_OBJS)
	$(AR) libmlton-gdb.a $(DEBUG_OBJS)

# gcc 3.2 is buggy (or maybe we're not following the C spec)
# when compiling Real/*.c with -O2.
basis/Real/%.o: basis/Real/%.c
	$(CC) $(CFLAGS) -O1 -c -o $@ $<

%-gdb.o: %.c
	$(CC) $(DEBUGFLAGS) -DASSERT=1 -c -o $@ $<

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

%-gdb.o: %.S
	$(CC) $(DEBUGFLAGS) -c -o $@ $<

%.o: %.S
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: clean
clean:
	../bin/clean

.PHONY: depend
depend:
	makedepend -f- -- $(CFLAGS) -- $(SRCS)

.PHONY: gdtoa-patch
gdtoa-patch:
	cd gdtoa && make clean && rm -f &~
	mv gdtoa gdtoa-new
	zcat gdtoa.tgz | tar xf -
	diff -P -C 2 -r gdtoa gdtoa-new >gdtoa-patch || exit 0
	rm -rf gdtoa
	mv gdtoa-new gdtoa
