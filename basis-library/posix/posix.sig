signature POSIX =
   sig
      structure Error: POSIX_ERROR
      structure FileSys: POSIX_FILE_SYS
      structure IO: POSIX_IO
      structure ProcEnv: POSIX_PROC_ENV
      structure Process: POSIX_PROCESS
      structure Signal: POSIX_SIGNAL
      structure SysDB: POSIX_SYS_DB
      structure TTY: POSIX_TTY

      sharing type FileSys.file_desc = ProcEnv.file_desc = IO.file_desc = TTY.file_desc
      sharing type ProcEnv.gid = FileSys.gid = SysDB.gid
      sharing type FileSys.open_mode = IO.open_mode
      sharing type Process.pid = ProcEnv.pid = IO.pid = TTY.pid
      sharing type Process.signal = Signal.signal
      sharing type ProcEnv.uid = FileSys.uid = SysDB.uid
   end

signature POSIX_EXTRA =
   sig
      structure Error: POSIX_ERROR_EXTRA
      structure FileSys: POSIX_FILE_SYS_EXTRA
      structure IO: POSIX_IO
      structure ProcEnv: POSIX_PROC_ENV
      structure Process: POSIX_PROCESS_EXTRA
      structure Signal: POSIX_SIGNAL_EXTRA
      structure SysDB: POSIX_SYS_DB
      structure TTY: POSIX_TTY

      sharing type FileSys.file_desc = ProcEnv.file_desc = IO.file_desc = TTY.file_desc
      sharing type ProcEnv.gid = FileSys.gid = SysDB.gid
      sharing type FileSys.open_mode = IO.open_mode
      sharing type Process.pid = ProcEnv.pid = IO.pid = TTY.pid
      sharing type Process.signal = Signal.signal
      sharing type ProcEnv.uid = FileSys.uid = SysDB.uid
   end
