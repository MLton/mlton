(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure PosixError: POSIX_ERROR_EXTRA =
   struct
      structure Prim = PosixPrimitive.Error
      open Prim
	 
      exception SysErr of string * syserror option

      val toWord = Word.fromInt
      val fromWord = Word.toInt

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
	 in if Primitive.Cpointer.isNull cs
	       then "Unknown error"
	    else C.CS.toString cs
	 end

      fun raiseSys n = raise SysErr (errorMsg n, SOME n)

      fun restart (f: 'a -> int) (a: 'a): int =
	 let
	    fun loop () =
	       case f a of
		  ~1 => let val errno = getErrno ()
			in if errno = intr
			      then loop ()
			   else raiseSys errno
			end
		| n => n
	 in loop ()
	 end
      
      fun error () = raiseSys (getErrno ())

      fun checkReturnResult (n: int) = if n = ~1 then error () else n
      fun checkResult n = (checkReturnResult n; ())
   end
