#include "platform.h"

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

	if (0 == GetTempPath(size, file_path))
		diee ("unable to make temporary file");
	strncpy (templ, template, 3);
	templ[4] = 0x00;
	if (0 == GetTempFileName (file_path, templ, 0, file_name))
		diee ("unable to make temporary file");
	return _open (file_name, _O_CREAT | _O_RDWR, _S_IREAD | _S_IWRITE);
}

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

Word32 totalRam (GC_state s) {
	MEMORYSTATUS memStat;

	memStat.dwLength = sizeof(memStat);
	GlobalMemoryStatus(&memStat);
	return memStat.dwTotalPhys;
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
/*                   Posix.FileSys                   */
/* ------------------------------------------------- */

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

int fchmod (int filedes, mode_t mode) {
	char fname[MAX_PATH + 1];

	GetWin32FileName (filedes, fname);
	return _chmod (fname, mode);
}

int lstat (const char *file_name, struct stat *buf) {
	/* Win32 doesn't really have links. */
	return stat (file_name, buf);
}

int mkdir2 (const char *pathname, mode_t mode) {
	return mkdir (pathname);
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

pid_t waitpid (pid_t pid, int *status, int options) {
	return _cwait (status, pid, options);
}

/* ------------------------------------------------- */
/*                      Signals                      */
/* ------------------------------------------------- */

int sigismember (const sigset_t *set, const int signum) {
	if (signum < 0 or signum >= NSIG) {
		errno = EINVAL;
		return -1;
	}
	return (*set & SIGTOMASK(signum)) ? 1 : 0;
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

/* ------------------------------------------------- */
/*                Posix.SysDB.Passwd                 */
/* ------------------------------------------------- */

#define INFO_LEVEL 3
static LPUSER_INFO_3 usrData = NULL;

static struct passwd passwd;

struct passwd *getpwnam (const char *name) {
	unless (NERR_Success == 
			NetUserGetInfo (NULL, (LPCWSTR)name, INFO_LEVEL, 
					(LPBYTE*)&usrData))
		return NULL;
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
/*                      Socket                       */
/* ------------------------------------------------- */

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
