signature POSIX_ERROR =
   sig
      eqtype syserror

      val toWord: syserror -> SysWord.word
      val fromWord: SysWord.word -> syserror

      val errorMsg: syserror -> string
      val errorName: syserror -> string
      val syserror: string -> syserror option

      val acces: syserror
      val again: syserror
      val badf: syserror
      val badmsg: syserror
      val busy: syserror
      val canceled: syserror
      val child: syserror
      val deadlk: syserror
      val dom: syserror
      val exist: syserror
      val fault: syserror
      val fbig: syserror
      val inprogress: syserror
      val intr: syserror
      val inval: syserror
      val io: syserror
      val isdir: syserror
      val loop: syserror
      val mfile: syserror
      val mlink: syserror
      val msgsize: syserror
      val nametoolong: syserror
      val nfile: syserror
      val nodev: syserror
      val noent: syserror
      val noexec: syserror
      val nolck: syserror
      val nomem: syserror
      val nospc: syserror
      val nosys: syserror
      val notdir: syserror
      val notempty: syserror
      val notsup: syserror
      val notty: syserror
      val nxio: syserror
      val perm: syserror
      val pipe: syserror
      val range: syserror
      val rofs: syserror
      val spipe: syserror
      val srch: syserror
      val toobig: syserror
      val xdev: syserror

   end

signature POSIX_ERROR_EXTRA =
   sig
      include POSIX_ERROR

      exception SysErr of string * syserror option

      val raiseSys: syserror -> 'a
	 
      (* raises SysErr with ERRNO *)
      (* raiseSys if arg is -1 *)
      val checkResult: int -> unit
      val checkReturnResult: int -> int 
      val checkReturnPosition: Position.int -> Position.int
      val getErrno: unit -> int
      val clearErrno: unit -> unit
      val error: unit -> 'a
   end
