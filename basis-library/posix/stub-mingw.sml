(* Stub out functions that are not implemented on MinGW. *)
local
   structure Error = PosixError
   val stub: ('a -> 'b) -> ('a -> 'b) =
      fn f => 
      if let open Primitive.MLton.Platform.OS
	 in MinGW = host
	 end
	 then fn _ => Error.raiseSys Error.nosys
      else f
in
   structure PosixPrimitive =
      struct
	 open PosixPrimitive

	 structure FileSys =
	    struct
	       open FileSys

	       val chown = stub chown
	       val fchown = stub fchown
	       val fpathconf = stub fpathconf
	       val ftruncate = stub ftruncate
	       val link = stub link
	       val mkfifo = stub mkfifo
	       val pathconf = stub pathconf
	       val readlink = stub readlink
	       val symlink = stub symlink
	    end

	 structure IO =
	    struct
	       open IO
		  
	       val fcntl2 = stub fcntl2
	       val fcntl3 = stub fcntl3
	    end

	 structure ProcEnv =
	    struct
	       open ProcEnv

	       structure Uname =
		  struct
		     open Uname

		     val uname = stub uname
		  end

	       val ctermid = stub ctermid
	       val getegid = stub getegid
	       val geteuid = stub geteuid
	       val getgid = stub getgid
	       val getgroups = stub getgroups
	       val getlogin = stub getlogin
	       val getpgrp = stub getpgrp
	       val getpid = stub getpid
	       val getppid = stub getppid
	       val getuid = stub getuid
	       val setgid = stub setgid
	       val setpgid = stub setpgid
	       val setsid = stub setsid
	       val setuid = stub setuid
	       val sysconf = stub sysconf
	       val times = stub times
	       val ttyname = stub ttyname
	    end

	 structure SysDB =
	    struct
	       open SysDB
		  
	       val getgrgid = stub getgrgid
	       val getgrnam = stub getgrnam
	       val getpwuid = stub getpwuid
	    end

	 structure TTY =
	    struct
	       open TTY
		  
	       val drain = stub drain
	       val flow = stub flow
	       val flush = stub flush
	       val getattr = stub getattr
	       val getpgrp = stub getpgrp
	       val sendbreak = stub sendbreak
	       val setattr = stub setattr
	       val setpgrp = stub setpgrp
	    end
      end
   
   structure Primitive =
      struct
	 open Primitive

	 structure Itimer =
	    struct
	       open Itimer

	       val set = stub set
	    end

	 structure MLton =
	    struct
	       open MLton
		  
	       structure Rusage =
		  struct
		     open Rusage

		     val ru = stub ru
		  end
	    end

	 structure OS =
	    struct
	       open OS

	       structure IO =
		  struct
		     open IO

		     val poll = stub poll
		  end
	    end

	 structure Socket =
	    struct
	       open Socket

	       structure UnixSock =
		  struct
		     open UnixSock

		     val toAddr = stub toAddr
		     val fromAddr = stub fromAddr
		  end
	    end
      end
end
