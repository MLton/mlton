(* Copyright (C) 1999-2005 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
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
