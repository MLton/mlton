structure MLtonProcess: MLTON_PROCESS =
   struct
      structure Prim = Primitive.MLton.Process
      structure Error = PosixError
      structure MLton = Primitive.MLton

      type pid = Posix.Process.pid

      fun spawne {path, args, env} =
	 if MLton.hostType = MLton.Cygwin
	    then Error.checkReturnResult (Prim.spawne (String.nullTerm path,
						       C.CSS.fromList args,
						       C.CSS.fromList env))
	 else
	    case Posix.Process.fork () of
	       NONE => Posix.Process.exece (path, args, env)
	     | SOME pid => pid

      fun spawn {path, args} =
	 spawne {path = path, args = args, env = Posix.ProcEnv.environ ()}

      fun spawnp {file, args} =
	 if MLton.hostType = MLton.Cygwin
	    then Error.checkReturnResult (Prim.spawnp (String.nullTerm file,
						       C.CSS.fromList args))
	 else	 
	    case Posix.Process.fork () of
	       NONE => Posix.Process.execp (file, args)
	     | SOME pid => pid

      fun exit sts =
	 if 0 <= sts andalso sts < 256
	    then (let open Cleaner in clean atExit end
		  ; Primitive.halt sts
                  ; raise Fail "exit")
	 else raise Fail (concat ["exit must have 0 <= status < 256: saw ",
				  Int.toString sts])
   end

