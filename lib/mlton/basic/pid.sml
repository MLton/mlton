structure Pid: PID =
   struct
      local open Posix.Process
      in
	 type t = pid
	 val toString = SysWord.fmt StringCvt.DEC o pidToWord
	 fun fromString s =
	    Option.map(Pervasive.Int.fromString s, wordToPid o SysWord.fromInt)
      end


      val layout = Layout.str o toString

      local open Posix.ProcEnv
      in
	 val current = getpid
	 val parent = getppid
      end

      val equals = op =
   end
