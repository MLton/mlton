structure MLtonProcess =
   struct
      structure Prim = Primitive.MLton.Process
      structure Error = PosixError
      structure MLton = Primitive.MLton

      type pid = Pid.t

      val isCygwin = let open MLton.Platform.OS in host = Cygwin end
	 
      fun spawne {path, args, env} =
	 if isCygwin
	    then
	       let
		  val pid = Prim.spawne (String.nullTerm path,
					 C.CSS.fromList args,
					 C.CSS.fromList env)
		  val _ = Error.checkResult (Pid.toInt pid)
	       in
		  pid
	       end
	 else
	    case Posix.Process.fork () of
	       NONE => Posix.Process.exece (path, args, env)
	     | SOME pid => pid

      fun spawn {path, args} =
	 spawne {path = path, args = args, env = Posix.ProcEnv.environ ()}

      fun spawnp {file, args} =
	 if isCygwin
	    then
	       let
		  val pid = Prim.spawnp (String.nullTerm file,
					 C.CSS.fromList args)
		  val _ = Error.checkResult (Pid.toInt pid)
	       in
		  pid
	       end
	 else	 
	    case Posix.Process.fork () of
	       NONE => Posix.Process.execp (file, args)
	     | SOME pid => pid

      val exiting = ref false

      exception Exit
      
      fun exit (status: int): 'a =
	 if !exiting
	    then raise Exit
	 else
	    (exiting := true
	     ; if 0 <= status andalso status < 256
		  then (let open Cleaner in clean atExit end
			; Primitive.halt status
			; raise Fail "exit")
	       else raise Fail (concat ["exit must have 0 <= status < 256: saw ",
					Int.toString status]))

      fun atExit f =
	 if !exiting
	    then ()
	 else Cleaner.addNew (Cleaner.atExit, f)
   end

