#include "platform.h"

#include "showMem.win32.c"

int getpagesize (void) {
	SYSTEM_INFO sysinfo;
	GetSystemInfo(&sysinfo);
	return sysinfo.dwPageSize;
}

int mkstemp (char *template) {
	char file_path[255];
	char file_name[255];
	char templ[4];
	DWORD size = sizeof(file_path);

	if (0 == GetTempPath (size, file_path))
		diee ("unable to make temporary file");
	strncpy (templ, template, 3);
	templ[4] = 0x00;
	if (0 == GetTempFileName (file_path, templ, 0, file_name))
		diee ("unable to make temporary file");
	return _open (file_name, _O_CREAT | _O_RDWR, _S_IREAD | _S_IWRITE);
}

Word32 totalRam (GC_state s) {
	MEMORYSTATUS memStat;

	memStat.dwLength = sizeof(memStat);
	GlobalMemoryStatus(&memStat);
	return memStat.dwTotalPhys;
}

/* ------------------------------------------------- */
/*                       Date                        */
/* ------------------------------------------------- */

#ifndef __GNUC__
#define EPOCHFILETIME (116444736000000000i64)
#else
#define EPOCHFILETIME (116444736000000000LL)
#endif

/* Based on notes by Wu Yongwei: 
 *   http://mywebpage.netscape.com/yongweiwutime.htm 
 */
int gettimeofday (struct timeval *tv, struct timezone *tz) {
	FILETIME ft;
	LARGE_INTEGER li;
	__int64 t;
	static bool tzInit = FALSE;
	
	unless (tzInit) {
		tzInit = TRUE;
		_tzset();
	}
	GetSystemTimeAsFileTime (&ft);
	li.LowPart = ft.dwLowDateTime;
	li.HighPart = ft.dwHighDateTime;
	t = li.QuadPart;
	t -= EPOCHFILETIME;
	t /= 10;
	tv->tv_sec = (long)(t / 1000000);
	tv->tv_usec = (long)(t % 1000000);
	return 0;
}

/* ------------------------------------------------- */
/*                   MLton.Itimer                    */
/* ------------------------------------------------- */

int setitimer (int which, 
		const struct itimerval *value, 
		struct itimerval *ovalue) {
	die ("setitimer not implemented");
}

/* ------------------------------------------------- */
/*                   MLton.Rlimit                    */
/* ------------------------------------------------- */

static struct rlimit rlimits[RLIM_NLIMITS];

static void initRlimits () {
	static int done = FALSE;
 	int lim;

	if (done)
		return;
	done = TRUE;
	for (lim = 0; lim < RLIM_NLIMITS; ++lim ) {
		rlimits[lim].rlim_cur = 0;
		rlimits[lim].rlim_max = UINT_MAX;
	}
}

int getrlimit (int resource, struct rlimit *rlp) {
	initRlimits ();
	if (resource < 0 or resource >= RLIM_NLIMITS) {
		errno = EINVAL;
		return -1;
	}
	*rlp = rlimits[resource];
	return 0;
}

int setrlimit (int resource, const struct rlimit *rlp) {
	initRlimits ();
	if (resource < 0 or resource >= RLIM_NLIMITS) {
		errno = EINVAL;
		return -1;
	}
	if (rlp->rlim_cur < rlimits[resource].rlim_max)
		rlimits[resource].rlim_cur = rlp->rlim_cur;
	else {
		errno = EPERM;
		return -1;
	}
	rlimits[resource].rlim_max = rlp->rlim_max;
	return 0;
}

/* ------------------------------------------------- */
/*                   MLton.Rusage                    */
/* ------------------------------------------------- */

int getrusage (int who, struct rusage *usage) {
	die ("getrusage not implemented");
}

/* ------------------------------------------------- */
/*                       OS.IO                       */
/* ------------------------------------------------- */

int poll (struct pollfd *ufds, unsigned int nfds, int timeout) {
	die ("poll not implemented");
}

/* ------------------------------------------------- */
/*                   Posix.FileSys                   */
/* ------------------------------------------------- */

#if FALSE
static void GetWin32FileName (int fd, char* fname) {
	HANDLE fh, fhmap;
	DWORD fileSize, fileSizeHi;
	void* pMem = NULL;
	
	fh = (HANDLE)_get_osfhandle (fd);
	fileSize = GetFileSize (fh, &fileSizeHi);
	fhmap = CreateFileMapping (fh, NULL, PAGE_READONLY, 0, fileSize, NULL);
	if (fhmap) {
	    pMem = MapViewOfFile (fhmap, FILE_MAP_READ, 0, 0, 1);
	    if (pMem) {
		GetMappedFileNameA (GetCurrentProcess(), pMem, fname, MAX_PATH);
		UnmapViewOfFile (pMem);
	    }
	    CloseHandle (fhmap);
	}
	return;	
}
#endif

int chown (const char *path, uid_t owner, gid_t group) {
	die ("chown not implemented");
}

int fchmod (int filedes, mode_t mode) {
	die ("chown not implemented");
//	char fname[MAX_PATH + 1];
//
//	GetWin32FileName (filedes, fname);
//	return _chmod (fname, mode);
}

int fchown (int fd, uid_t owner, gid_t group) {
	die ("fchown not implemented");
}

long fpathconf (int filedes, int name) {
	die ("fpathconf not implemented");
}

int ftruncate (int fd, off_t length) {
	die ("ftruncate not implemented");
}

int link (const char *oldpath, const char *newpath) {
	die ("link not implemented");
}

int lstat (const char *file_name, struct stat *buf) {
	/* Win32 doesn't really have links. */
	return stat (file_name, buf);
}

int mkdir2 (const char *pathname, mode_t mode) {
	return mkdir (pathname);
}

int mkfifo (const char *pathname, mode_t mode) {
	die ("mkfifo not implemented");
}

long pathconf (char *path, int name) {
	die ("pathconf not implemented");
}

int readlink (const char *path, char *buf, size_t bufsiz) {
	die ("readlink not implemented");
}

int symlink (const char *oldpath, const char *newpath) {
	die ("symlink not implemented");
}

/* ------------------------------------------------- */
/*                     Posix.IO                      */
/* ------------------------------------------------- */

int fcntl (int fd, int cmd, ...) {
	die ("fcntl not implemented");
}

int fsync (int fd) {
	die ("fsync not implemented");
}

int pipe (int filedes[2]) {
	die ("pipe not implemented");
}

/* ------------------------------------------------- */
/*                   Posix.ProcEnv                   */
/* ------------------------------------------------- */

char *ctermid (char *s) {
	die ("*ctermid not implemented");
}
gid_t getegid (void) {
	die ("getegid not implemented");
}
uid_t geteuid (void) {
	die ("geteuid not implemented");
}
gid_t getgid (void) {
	die ("getgid not implemented");
}
int getgroups (int size, gid_t list[]) {
	die ("getgroups not implemented");
}
char *getlogin (void) {
	die ("*getlogin not implemented");
}
pid_t getpgid(pid_t pid) {
	die ("getpgid not implemented");
}
pid_t getpgrp(void) {
	die ("getpgrp not implemented");
}
pid_t getpid (void) {
	die ("getpid not implemented");
}
pid_t getppid (void) {
	die ("getppid not implemented");
}
uid_t getuid (void) {
	die ("getuid not implemented");
}
int setenv (const char *name, const char *value, int overwrite) {
	die ("setenv not implemented");
}
int setgid (gid_t gid) {
	die ("setgid not implemented");
}
pid_t setsid (void) {
	die ("setsid not implemented");
}
int setuid (uid_t uid) {
	die ("setuid not implemented");
}
long sysconf (int name) {
	die ("sysconf not implemented");
}
clock_t times (struct tms *buf) {
	die ("times not implemented");
}
char *ttyname (int desc) {
	die ("*ttyname not implemented");
}
int uname (struct utsname *buf) {
	die ("uname not implemented");
}

/* ------------------------------------------------- */
/*                   Posix.Process                   */
/* ------------------------------------------------- */

static UINT_PTR curr_timer = 0;
static int curr_timer_dur = 0;
static LARGE_INTEGER timer_start_val;

VOID CALLBACK alarm_signalled(HWND window, UINT message,
	UINT_PTR timer_id, DWORD time)
{
    printf("Timer fired\n");
}

/*
 * Win32 alarm implementation
 */
int alarm (int secs) {
	LARGE_INTEGER timer_end_val, frequency;
	int remaining = 0;
	long elapse = secs * 1000;	/* win32 uses usecs */
    
	/* Unsetting the alarm */
	if (secs == 0 && curr_timer == 0) {
	    return 0;
	}
	if (curr_timer != 0) {
		KillTimer(0, curr_timer);
		QueryPerformanceCounter(&timer_end_val);
		QueryPerformanceFrequency(&frequency);
		if (frequency.QuadPart != 0) {
			remaining = curr_timer_dur - ((int)(timer_end_val.QuadPart
				- timer_start_val.QuadPart)/frequency.QuadPart);
			if (remaining < 0) {
				remaining = 0;
			}
		}

		timer_start_val.QuadPart = 0;
		curr_timer_dur = 0;
		curr_timer = 0;
	}
	if (secs != 0) {
		/* Otherwise, set a timer */
		curr_timer = SetTimer(0, 0, elapse, alarm_signalled);
		QueryPerformanceCounter(&timer_start_val);
		curr_timer_dur = secs;
	}
	return remaining;
}

pid_t fork (void) {
	die ("fork not implemented");
}

int kill (pid_t pid, int sig) {
	die ("kill not implemented");
}

int pause (void) {
	die ("pause not implemented");
}

unsigned int sleep (unsigned int seconds) {
	die ("int not implemented");
}

pid_t wait (int *status) {
	die ("wait not implemented");
}

pid_t waitpid (pid_t pid, int *status, int options) {
	return _cwait (status, pid, options);
}

/* ------------------------------------------------- */
/*                      Signals                      */
/* ------------------------------------------------- */

int sigaction (int signum, 
			const struct sigaction *newact,
			struct sigaction *oldact) {

	struct sigaction oa;

	if (signum < 0 or signum >= NSIG) {
		errno = EINVAL;
		return -1;
	}
	if (newact) {
		if (signum == SIGKILL || signum == SIGSTOP) {
			errno = EINVAL;
			return -1;
		}
		oa.sa_handler = signal (signum, newact->sa_handler);
	}
	if (oldact)
		oldact->sa_handler = oa.sa_handler;
	return 0;
}

int sigaddset (sigset_t *set, const int signum) {
	if (signum < 0 or signum >= NSIG) {
		errno = EINVAL;
		return -1;
	}
	*set |= SIGTOMASK (signum);
	return 0;
}

int sigdelset (sigset_t *set, const int signum) {
	if (signum < 0 or signum >= NSIG) {
		errno = EINVAL;
		return -1;
	}
	*set &= ~SIGTOMASK (signum);
	return 0;
}

int sigemptyset (sigset_t *set) {
	*set = (sigset_t) 0;
	return 0;
}

int sigfillset (sigset_t *set) {
	*set = ~((sigset_t) 0);
	return 0;
}

int sigismember (const sigset_t *set, const int signum) {
	if (signum < 0 or signum >= NSIG) {
		errno = EINVAL;
		return -1;
	}
	return (*set & SIGTOMASK(signum)) ? 1 : 0;
}

int sigpending (sigset_t *set) {
	die ("sigpending not implemented");
}

int sigprocmask (int how, const sigset_t *set, sigset_t *oldset) {

	sigset_t opmask;

	if (oldset) {
		//*oldset = opmask;
	}
	if (set) {
		sigset_t newmask = opmask;

		switch (how) {
			case SIG_BLOCK:
				/* add set to current mask */
				newmask |= *set;
			break;
			case SIG_UNBLOCK:
				/* remove set from current mask */
				newmask &= ~*set;
			break;
			case SIG_SETMASK:
				/* just set it */
				newmask = *set;
			break;
			default:
				return -1;
		}
		//(void) set_signal_mask (newmask, opmask);
	}
	return 0;
}

int sigsuspend (const sigset_t *mask) {
	die ("sigsuspend not implemented");
}

/* ------------------------------------------------- */
/*                Posix.SysDB.Passwd                 */
/* ------------------------------------------------- */

#define INFO_LEVEL 3
static LPUSER_INFO_3 usrData = NULL;

static struct passwd passwd;

struct group *getgrgid (gid_t gid) {
	die ("getgrgid not implemented");
}

struct group *getgrnam (const char *name) {
	die ("getgrnam not implemented");
}

struct passwd *getpwnam (const char *name) {
	return NULL;
//	unless (NERR_Success == 
//			NetUserGetInfo (NULL, (LPCWSTR)name, INFO_LEVEL, 
//					(LPBYTE*)&usrData))
//		return NULL;
	passwd.pw_dir = (char*)usrData->usri3_home_dir;
	passwd.pw_gid = usrData->usri3_primary_group_id;
	passwd.pw_name = (char*)usrData->usri3_name;
	passwd.pw_shell = (char*)usrData->usri3_script_path;
	passwd.pw_uid = usrData->usri3_user_id;
	return &passwd;
}

struct passwd *getpwuid (uid_t uid) {
	die ("getpwuid not implemented");
}

/* ------------------------------------------------- */
/*                     Posix.TTY                     */
/* ------------------------------------------------- */

speed_t cfgetispeed (struct termios *termios_p) {
	die ("cfgetispeed not implemented");
}

speed_t cfgetospeed (struct termios *termios_p) {
	die ("cfgetospeed not implemented");
}

int cfsetispeed (struct termios *termios_p, speed_t speed) {
	die ("cfsetispeed not implemented");
}

int cfsetospeed (struct termios *termios_p, speed_t speed) {
	die ("cfsetospeed not implemented");
}

int tcdrain (int fd) {
	die ("tcdrain not implemented");
}

int tcflow (int fd, int action) {
	die ("tcflow not implemented");
}

int tcflush (int fd, int queue_selector) {
	die ("tcflush not implemented");
}

int tcgetattr (int fd, struct termios *termios_p) {
	die ("tcgetattr not implemented");
}

pid_t tcgetpgrp (int fd) {
	die ("tcgetpgrp not implemented");
}

int tcsendbreak (int fd, int duration) {
	die ("tcsendbreak not implemented");
}

int tcsetattr (int fd, int optional_actions, struct termios *termios_p) {
	die ("tcsetattr not implemented");
}

int tcsetpgrp (int fd, pid_t pgrpid) {
	die ("tcsetpgrp not implemented");
}

/* ------------------------------------------------- */
/*                      Socket                       */
/* ------------------------------------------------- */

int ioctl (int d, int request, ...) {
	die ("ioctl not implemented");
}

int socketpair (int d, int type, int protocol, int sv[2]) {
	die ("socketpair not implemented");
}

void MLton_initSockets () {
	static Bool isInitialized = FALSE;
	WORD version;
	WSADATA wsaData;
	
	unless (isInitialized) {
		isInitialized = TRUE;
		version = MAKEWORD (2,2);
		WSAStartup (version, &wsaData);
	}
}
