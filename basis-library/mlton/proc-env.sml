(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

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

      fun setgroups gs =
	 PosixError.SysCall.simple
	 (fn () => PosixPrimitive.ProcEnv.setgroups (Array.fromList gs))
   end
