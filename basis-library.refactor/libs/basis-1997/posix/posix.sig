signature POSIX_1997 =
  sig
    structure Error: POSIX_ERROR
    structure Signal: POSIX_SIGNAL
    structure Process: POSIX_PROCESS_1997
    structure ProcEnv: POSIX_PROC_ENV
    structure FileSys: POSIX_FILE_SYS_1997
    structure IO: POSIX_IO_1997
    structure SysDB: POSIX_SYS_DB
    structure TTY: POSIX_TTY_1997
  end
