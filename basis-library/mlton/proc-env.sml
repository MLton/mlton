structure MLtonProcEnv: MLTON_PROC_ENV =
   struct
      fun setenv {name, value} =
	 PosixError.checkResult
	 (PosixPrimitive.ProcEnv.setenv
	  (String.nullTerm name, String.nullTerm value))
   end
