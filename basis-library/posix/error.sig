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

      val cleared: syserror
      val raiseSys: syserror -> 'a
      val raiseSysWithMsg: syserror * string -> 'a

      structure SysCall :
         sig
            val blocker: (unit -> (unit -> unit)) ref
            val restartFlag: bool ref

            val syscallErr: 
               {clear: bool, restart: bool, errVal: ''a} * 
               (unit -> {return: ''a C_Errno.t,
                         post: ''a -> 'b,
                         handlers: (syserror * (unit -> 'b)) list}) -> 'b

            (* clear = false, restart = false, errVal = ~1
             * post = fn _ => (), handlers = []
             *)
            val simple: (unit -> C_Int.t C_Errno.t) -> unit
            (* clear = false, restart = false, 
             * post = fn _ => (), handlers = []
             *)
            val simple': {errVal: ''a} * (unit -> ''a C_Errno.t) -> unit

            (* clear = false, restart = true, errVal = ~1
             * post = fn _ => (), handlers = []
             *)
            val simpleRestart: (unit -> C_Int.t C_Errno.t) -> unit
            (* clear = false, restart = true, 
             * post = fn _ => (), handlers = []
             *)
            val simpleRestart': {errVal: ''a} * (unit -> ''a C_Errno.t) -> unit

            (* clear = false, restart = false, errVal = ~1
             * post = fn ret => ret, handlers = []
             *)
            val simpleResult: (unit -> C_Int.t C_Errno.t) -> C_Int.t
            (* clear = false, restart = false, 
             * post = fn ret => ret, handlers = []
             *)
            val simpleResult': {errVal: ''a} * (unit -> ''a C_Errno.t) -> ''a

            (* clear = false, restart = true, errVal = ~1
             * post = fn ret => ret, handlers = []
             *)
            val simpleResultRestart: (unit -> C_Int.t C_Errno.t) -> C_Int.t
            (* clear = false, restart = true, 
             * post = fn ret => ret, handlers = []
             *)
            val simpleResultRestart': {errVal: ''a} * (unit -> ''a C_Errno.t) -> ''a

            (* clear = false, restart = false, errVal = ~1
             * handlers = []
             *)
            val syscall: (unit -> C_Int.t C_Errno.t * (C_Int.t -> 'a)) -> 'a
            (* clear = false, restart = false, 
             * handlers = []
             *)
            val syscall': {errVal: ''a} * (unit -> ''a C_Errno.t * (''a -> 'b)) -> 'b

            (* clear = false, restart = true, errVal = ~1
             * handlers = []
             *)
            val syscallRestart: (unit -> C_Int.t C_Errno.t * (C_Int.t -> 'a)) -> 'a
            (* clear = false, restart = true, 
             * handlers = []
             *)
            val syscallRestart': {errVal: ''a} * (unit -> ''a C_Errno.t * (''a -> 'b)) -> 'b
         end
   end
