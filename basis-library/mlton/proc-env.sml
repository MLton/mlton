structure MLtonProcEnv: MLTON_PROC_ENV =
   struct
      fun setenv {name, value} =
	 let
	    val name = NullString.nullTerm name
	    val value = NullString.nullTerm value
	 in
	    PosixError.SysCall.simple
	    (fn () => PosixPrimitive.ProcEnv.setenv (name, value))
	 end
   end
