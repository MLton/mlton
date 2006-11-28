signature OS_1997 =
   sig
      eqtype syserror

      exception SysErr of string * syserror option

      val errorMsg: syserror -> string 
      val errorName: syserror -> string
      val syserror: string -> syserror option

      structure FileSys: OS_FILE_SYS_1997
      structure Path: OS_PATH_1997
      structure Process: OS_PROCESS_1997
      structure IO: OS_IO
   end
