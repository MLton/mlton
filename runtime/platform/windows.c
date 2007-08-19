HANDLE fileDesHandle (int fd);

#define BUFSIZE 65536

static HANDLE tempFileDes (void) {
  /* Based on http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/creating_and_using_a_temporary_file.asp
   */  
  HANDLE hTempFile; 
  DWORD dwRetVal;
  DWORD dwBufSize=BUFSIZE;
  UINT uRetVal;
  char szTempName[BUFSIZE];  
  char lpPathBuffer[BUFSIZE];

  dwRetVal = GetTempPath(dwBufSize, lpPathBuffer);
  if (dwRetVal > dwBufSize)
    die ("GetTempPath failed with error %ld\n", GetLastError());
  uRetVal = GetTempFileName(lpPathBuffer, "TempFile", 0, szTempName);
  if (0 == uRetVal)
    die ("GetTempFileName failed with error %ld\n", GetLastError());
  hTempFile = CreateFile((LPTSTR) szTempName, GENERIC_READ | GENERIC_WRITE,
    0, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL | FILE_FLAG_DELETE_ON_CLOSE,
    NULL);                
  if (hTempFile == INVALID_HANDLE_VALUE)
    die ("CreateFile failed with error %ld\n", GetLastError());
  return hTempFile;
}

typedef struct {
  HANDLE handle;
} *WriteToDiskData;

void GC_diskBack_read (void *data, pointer buf, size_t size) {
  HANDLE h;
  DWORD d;
  DWORD dwBytesRead;

  h = ((WriteToDiskData)data)->handle;
  d = SetFilePointer (h, 0, NULL, FILE_BEGIN);
  if (d == INVALID_SET_FILE_POINTER)
    die ("SetFilePointer failed with error %ld\n", GetLastError());
  unless (ReadFile(h, buf, size, &dwBytesRead, NULL))
    die ("ReadFile failed with error %ld\n", GetLastError());
}

void GC_diskBack_close (void *data) {
  HANDLE h;

  h = ((WriteToDiskData)data)->handle;
  unless (CloseHandle (h))
    die ("CloseHandle failed with error %ld.", GetLastError());
  free (data);
}

void *GC_diskBack_write (pointer buf, size_t size) {
  HANDLE h;
  WriteToDiskData d;
  DWORD dwBytesWritten;

  h = tempFileDes ();
  unless (WriteFile (h, buf, size, &dwBytesWritten, NULL))
    die ("WriteFile failed with error %ld\n", GetLastError());
  d = (WriteToDiskData)(malloc_safe (sizeof(*d)));
  d->handle = h;
  return d;
}

static void displayMaps (void) {
        MEMORY_BASIC_INFORMATION buf;
        LPVOID lpAddress;
        const char *state = "<unset>";
        const char *protect = "<unset>";

        for (lpAddress = 0; lpAddress < (LPCVOID)0x80000000; ) {
                VirtualQuery (lpAddress, &buf, sizeof (buf));

                switch (buf.Protect) {
                case PAGE_READONLY:
                        protect = "PAGE_READONLY";
                        break;
                case PAGE_READWRITE:
                        protect = "PAGE_READWRITE";
                        break;
                case PAGE_WRITECOPY:
                        protect = "PAGE_WRITECOPY";
                        break;
                case PAGE_EXECUTE:
                        protect = "PAGE_EXECUTE";
                        break;
                case PAGE_EXECUTE_READ:
                        protect = "PAGE_EXECUTE_READ";
                        break;
                case PAGE_EXECUTE_READWRITE:
                        protect = "PAGE_EXECUTE_READWRITE";
                        break;
                case PAGE_EXECUTE_WRITECOPY:
                        protect = "PAGE_EXECUTE_WRITECOPY";
                        break;
                case PAGE_GUARD:
                        protect = "PAGE_GUARD";
                        break;
                case PAGE_NOACCESS:
                        protect = "PAGE_NOACCESS";
                        break;
                case PAGE_NOCACHE:
                        protect = "PAGE_NOCACHE";
                        break;
                default:
                        assert (FALSE);
                }
                switch (buf.State) {
                case MEM_COMMIT:
                        state = "MEM_COMMIT";
                        break;
                case MEM_FREE:
                        state = "MEM_FREE";
                        break;
                case MEM_RESERVE:
                        state = "MEM_RESERVE";
                        break;
                default:
                        assert (FALSE);
                }
                fprintf(stderr, "0x%8x %10u  %s %s\n",
                        (unsigned int)buf.BaseAddress,
                        (unsigned int)buf.RegionSize,
                        state, protect);
                lpAddress = (unsigned char*)lpAddress + buf.RegionSize;
        }
}

void GC_displayMem (void) {
        MEMORYSTATUS ms; 

        ms.dwLength = sizeof (MEMORYSTATUS); 
        GlobalMemoryStatus (&ms); 
        fprintf(stderr, "Total Phys. Mem: %ld\nAvail Phys. Mem: %ld\nTotal Page File: %ld\nAvail Page File: %ld\nTotal Virtual: %ld\nAvail Virtual: %ld\n",
                         ms.dwTotalPhys, 
                         ms.dwAvailPhys, 
                         ms.dwTotalPageFile, 
                         ms.dwAvailPageFile, 
                         ms.dwTotalVirtual, 
                         ms.dwAvailVirtual); 
        displayMaps ();
}

static HANDLE dupHandle (int fd) {
        HANDLE dupd;
        HANDLE raw;

        raw = fileDesHandle (fd);
        if (raw == (HANDLE)-1 or raw == 0) {
                errno = EBADF;
                return 0;
        }
        /* 'Inspired' by http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/creating_a_child_process_with_redirected_input_and_output.asp
         * It's interesting that you can open files for/from other processes...
         */
        unless (DuplicateHandle (
         GetCurrentProcess(),   /* source process */
         raw,                   /* source handle */
         GetCurrentProcess(),   /* target process */
         &dupd,                 /* target handle - valid in target proc */
         0,                     /* ignored b/c DUPLICATE_SAME_ACCESS used */
         TRUE,                  /* this can be inherited by children */
         DUPLICATE_SAME_ACCESS))/* keep the same permissions */
        {
                errno = ENOMEM;
                return 0;
        }
        return dupd;
}

static inline void Windows_decommit (void *base, size_t length) {
        if (0 == VirtualFree (base, length, MEM_DECOMMIT))
                die ("VirtualFree decommit failed");
}

static inline void *Windows_mmapAnon (__attribute__ ((unused)) void *start,
                                        size_t length) {
        void *res;

        /* Use "0" instead of "start" as the first argument to VirtualAlloc
         * because it is more stable on MinGW (at least).
         */
        res = VirtualAlloc ((LPVOID)0/*start*/, length, MEM_COMMIT, PAGE_READWRITE);
        if (NULL == res)
                res = (void*)-1;
        return res;
}

static inline void Windows_release (void *base) {
        if (0 == VirtualFree (base, 0, MEM_RELEASE))
                die ("VirtualFree release failed");
}

C_Errno_t(C_PId_t) 
Windows_Process_create (NullString8_t cmds, NullString8_t args, NullString8_t envs,
                        C_Fd_t in, C_Fd_t out, C_Fd_t err) {
        char    *cmd;
        char    *arg;
        char    *env;
        int     result;
        STARTUPINFO si;
        PROCESS_INFORMATION proc;

        cmd = (char*)cmds;
        arg = (char*)args;
        env = (char*)envs;
        memset (&proc, 0, sizeof (proc));
        memset (&si, 0, sizeof (si));
        si.cb = sizeof(si);
        si.hStdInput = dupHandle (in);
        si.hStdOutput = dupHandle (out);
        si.hStdError = dupHandle (err);
        si.dwFlags = STARTF_USESTDHANDLES; /* use the above */
        if (!si.hStdInput or !si.hStdOutput or !si.hStdError) {
                if (si.hStdInput) CloseHandle (si.hStdInput);
                if (si.hStdOutput) CloseHandle (si.hStdOutput);
                if (si.hStdError) CloseHandle (si.hStdError);
                /* errno already faked by create_dup_handle */
                return -1;
        }
        result = CreateProcess (
                 cmd,           /* The executable as a windows path */
                 arg,           /* Command-line as a string */
                 0,             /* Process inherits security params */
                 0,             /* Initial thread inherits security params */
                 TRUE,          /* Inherit HANDLEs set as inherit */
                 0,             /* Normal priority + no special flags */
                 env,           /* Environment as a string {n=v\0}\0 */
                 0,             /* Current directory = parent's */
                 &si,           /* Start info from above */
                 &proc);        /* returned handle */
        if (0 == result) {
                errno = ENOENT; /* probably does not exist (aka ENOFILE)*/
                result = -1;
        } else {
                /* Process created successfully */
                /* We will return the process handle for the 'pid'.
                 * This way we can TerminateProcess (kill) it and
                 * _cwait (waitpid) for it.
                 * The thread handle is not needed, so clean it.
                 */
                CloseHandle (proc.hThread);
                result = (int)proc.hProcess;
        }
        CloseHandle (si.hStdInput);
        CloseHandle (si.hStdOutput);
        CloseHandle (si.hStdError);
        return result;
}

C_Errno_t(C_Int_t) Windows_Process_terminate (C_PId_t pid, C_Signal_t sig) {
        HANDLE h;

        h = (HANDLE)pid;
        /* We terminate with 'sig' for the _return_ code + 0x80
         * Then in the basis library I test for this to decide W_SIGNALED.
         * Perhaps not the best choice, but I have no better idea.
         */
        unless (TerminateProcess (h, sig | 0x80)) {
                errno = ECHILD;
                return -1;
        }
        return 0;
}
