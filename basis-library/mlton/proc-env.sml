structure MLtonProcEnv: MLTON_PROC_ENV =
   struct
      fun setenv {name, value} =
	 PosixError.checkResult
	 (PosixPrimitive.ProcEnv.setenv
	  (NullString.nullTerm name, NullString.nullTerm value))
   end
