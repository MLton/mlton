(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure PosixError: POSIX_ERROR_EXTRA =
   struct
      structure Prim = PosixPrimitive.Error
      open Prim
	 
      exception SysErr of string * syserror option

      val toWord = SysWord.fromInt
      val fromWord = SysWord.toInt

      fun errorName n =
	 case List.find (fn (m, _) => n = m) errorNames of
	    NONE => "<UNKNOWN>"
	  | SOME (_, s) => s

      fun syserror s =
	 case List.find (fn (_, s') => s = s') errorNames of
	    NONE => NONE
	  | SOME (n, _) => SOME n

      fun errorMsg (n: int) =
	 let val cs = strerror n
	 in if Primitive.Pointer.isNull cs
	       then "Unknown error"
	    else C.CS.toString cs
	 end

      fun raiseSys n = raise SysErr (errorMsg n, SOME n)
      fun error () = raiseSys (getErrno ())
      fun checkReturnResult (n: int) = if n = ~1 then error () else n
      fun checkReturnPosition (n: Position.int) =
	 if n = ~1 then error () else n
      fun checkResult n = (checkReturnResult n; ())
   end
