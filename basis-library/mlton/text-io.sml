structure TextIO =
   struct
      open TextIO

      fun mkstemps {prefix, suffix}: string * outstream =
	 let
	    fun loop () =
	       let
		  val name = concat [prefix, Random.alphaNumString 6, suffix]
		  open Posix.FileSys
	       in
		  (name,
		   newOut (createf (name, O_WRONLY, O.flags [O.excl],
				    let open S
				    in flags [irusr, iwusr]
				    end)))
	       end handle e as PosixError.SysErr (_, SOME s) =>
		  if s = Posix.Error.exist
		     then loop ()
		  else raise e
	 in
	    loop ()
	 end

      fun mkstemp s = mkstemps {prefix = s, suffix = ""}
   end
