signature POSIX =
   sig
      structure Error: POSIX_ERROR
      structure Signal: POSIX_SIGNAL
      structure Process: POSIX_PROCESS
      structure ProcEnv: POSIX_PROC_ENV
      structure FileSys: POSIX_FILESYS
      structure IO: POSIX_IO
      structure SysDB: POSIX_SYS_DB
      structure TTY: POSIX_TTY
   end
