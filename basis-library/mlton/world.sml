structure World: MLTON_WORLD =
   struct
      structure Prim = Primitive.World
	 
      datatype status = Clone | Original

      (* Need to worry about:
       *   - open file descriptors
       *   - redetermine buffer status when restart
       *)
      fun save' (file: string): status =
	 let
	    val fd =
	       let
		  open Posix.FileSys
		  val flags =
		     let
			open S
		     in
			flags [irusr, iwusr, irgrp, iwgrp, iroth, iwoth]
		     end
	       in
		   (creat (file, flags))
	       end
	    val _ = Prim.save (Posix.FileSys.fdToWord fd)
	 in
	    if Prim.isOriginal ()
	       then (Posix.IO.close fd; Original)
	    else (Prim.makeOriginal ()
		  ; Cleaner.clean Cleaner.atLoadWorld
		  ; Clone)
	 end

      fun saveThread (file: string, t: unit Thread.t): unit =
	 case save' file of
	    Clone => Thread.switch (fn _ => (t, ()))
	  | Original => ()
	 
      fun save (file: string): status =
	 case !Thread.state of
	    Thread.Normal => save' file
	  | Thread.InHandler =>
	       raise Fail "cannot call MLton.World.save within signal handler"

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
