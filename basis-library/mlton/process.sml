structure Process:> MLTON_PROCESS =
   struct
      structure Prim = Primitive.MLton.Process
      structure Error = PosixError
      structure MLton = Primitive.MLton

      fun spawne {path, args, env} =
	 if MLton.hostType = MLton.Cygwin
	    then Error.checkResult (Prim.spawne (String.nullTerm path,
						 C.CSS.fromList args,
						 C.CSS.fromList env))
	 else
	    case Posix.Process.fork () of
	       NONE => Posix.Process.exece (path, args, env)
	     | SOME _ => ()

      fun spawn {path, args} =
	 spawne {path = path, args = args, env = Posix.ProcEnv.environ ()}

      fun spawnp {file, args} =
	 if MLton.hostType = MLton.Cygwin
	    then Error.checkResult (Prim.spawnp (String.nullTerm file,
						 C.CSS.fromList args))
	 else	 
	    case Posix.Process.fork () of
	       NONE => Posix.Process.execp (file, args)
	     | SOME _ => ()
   end
