structure World: MLTON_WORLD =
   struct
      structure Prim = Primitive.World
	 
      datatype status = Clone | Original

      (* Need to worry about:
       *   - open file descriptors
       *   - redetermine buffer status when restart
       *)
      fun save' (file: string,
		 f: (unit -> unit) -> unit): status =
	 (let open Cleaner in clean atSaveWorld end
	  ; (case Posix.Process.fork () of
		NONE =>
		   (Cleaner.clean Cleaner.atExit
		    ; f (fn () =>
			 (Prim.save (String.nullTerm file)
			  ; Cleaner.clean Cleaner.atLoadWorld))
		    ; Clone)
	      | SOME pid =>
		   let
		      open Posix.Process
		      val (pid', status) = waitpid (W_CHILD pid, [])
		   in
		      if pid = pid' andalso status = W_EXITED
			 then Original
		      else raise Fail (concat ["World.save ", file, "failed"])
		   end))

      fun saveThread (f: string, t: unit Thread.t) =
	 save' (f, fn save => Thread.switch' (fn _ => (t, save)))
	 
      fun save (f: string) =
	 case !Thread.state of
	    Thread.Normal => save' (f, fn save => save ())
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
