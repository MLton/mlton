structure MLtonProcess =
   struct
      structure Prim = Primitive.MLton.Process
      structure Error = PosixError
      structure SysCall = Error.SysCall
      structure MLton = Primitive.MLton

      type pid = Pid.t

      val useSpawn =
	 let
	    open MLton.Platform.OS
	 in
	    case host of
	       Cygwin => true
	     | MinGW => true
	     | _ => false
	 end
	 
      fun spawne {path, args, env} =
	 if useSpawn
	    then
	       let
		  val path = NullString.nullTerm path
		  val args = C.CSS.fromList args
		  val env = C.CSS.fromList env
	       in
		  SysCall.syscall
		  (fn () =>
		   let val pid = Prim.spawne (path, args, env)
		   in (Pid.toInt pid, fn () => pid)
		   end)
	       end
	 else
	    case Posix.Process.fork () of
	       NONE => Posix.Process.exece (path, args, env)
	     | SOME pid => pid

      fun spawn {path, args} =
	 spawne {path = path, args = args, env = Posix.ProcEnv.environ ()}

      fun spawnp {file, args} =
	 if useSpawn
	    then
	       let
		  val file = NullString.nullTerm file
		  val args = C.CSS.fromList args
	       in
		  SysCall.syscall
		  (fn () =>
		   let val pid = Prim.spawnp (file, args)
		   in (Pid.toInt pid, fn () => pid)
		   end)
	       end
	 else	 
	    case Posix.Process.fork () of
	       NONE => Posix.Process.execp (file, args)
	     | SOME pid => pid

      open Exit
   end

