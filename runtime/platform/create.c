static HANDLE dupHandle (int fd) {
	HANDLE raw, dupd;
	
	raw = (HANDLE)_get_osfhandle (fd);
	if (raw == (HANDLE)-1 || raw == 0) {
		errno = EBADF;
		return 0;
	}
	/* 'Inspired' by http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/creating_a_child_process_with_redirected_input_and_output.asp
	 * It's interesting that you can open files for/from other processes...
	 */
	unless (DuplicateHandle (
	 GetCurrentProcess(),	/* source process */
	 raw,			/* source handle */
	 GetCurrentProcess(),	/* target process */
	 &dupd,			/* target handle - valid in target proc */
	 0,			/* ignored b/c DUPLICATE_SAME_ACCESS used */
	 TRUE,			/* this can be inherited by children */
	 DUPLICATE_SAME_ACCESS))/* keep the same permissions */
	{
		errno = ENOMEM;
		return 0;
	}
	
	return dupd;
}

Pid MLton_Process_create (NullString cmds, NullString envs,
				Fd in, Fd out, Fd err) {
	char	*cmd;
	char	*env;
	int	result;
	STARTUPINFO si;
	PROCESS_INFORMATION proc;
	
	cmd = (char*)cmds;
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
		 0,		/* Obtain command from cmdline */
		 cmd,		/* Command-line as a string */
		 0,		/* Process inherits security params */
		 0,		/* Initial thread inherits security params */
		 TRUE,		/* Inherit HANDLEs set as inherit */
		 0,		/* Normal priority + no special flags */
		 env,	 	/* Environment as a string {n=v\0}\0 */
		 0,		/* Current directory = parent's */
		 &si,		/* Start info from above */
		 &proc);	/* returned handle */
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

