structure World: MLTON_WORLD =
   struct
      structure Prim = Primitive.World
	 
      datatype status = Clone | Original

      (* Need to worry about:
       *   - open file descriptors
       *   - redetermine buffer status when restart
       *)
      fun saveThread' (file: string,
		      f: status Thread.t -> status Thread.t): status =
	 (let open Cleaner in clean atSaveWorld end
	  ; (case Posix.Process.fork () of
		NONE =>
		   (let open Cleaner in clean atExit end
		    ; Thread.switch'
		       (fn t =>
			(f t,
			 fn () =>
			 (Prim.save (String.nullTerm file)
			  ; let open Cleaner in clean atLoadWorld end
			  ; Clone))))
	      | SOME pid =>
		   let
		      open Posix.Process
		      val (pid', status) = waitpid (W_CHILD pid, [])
		   in if pid = pid' andalso status = W_EXITED
			 then Original
		      else raise Fail (concat ["World.save ", file, "failed"])
		   end))

      fun saveThread (f, t) =
	 saveThread' (f, fn _ => Thread.prepend (t, fn _ => ()))
	 
      fun save f =
	 case !Thread.state of
	    Thread.Normal => saveThread' (f, fn t => t)
	  | Thread.InHandler t => saveThread (f, t)

      fun load (file: string): 'a =
	 if let open OS_FileSys
	    in access (file, [A_READ])
	    end
	    then 
	       let val c = CommandLine.name ()
	       in Posix.Process.exec (c, [c, "@MLton", "load-world", file, "--"])
	       end
	 else raise Fail (concat ["World.load can not read ", file])
   end
