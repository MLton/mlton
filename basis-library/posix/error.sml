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
	 let
	    val cs = strerror n
	 in
	    if cs = Primitive.Pointer.null
	       then "Unknown error"
	    else C.CS.toString cs
	 end

      fun raiseSys n = raise SysErr (errorMsg n, SOME n)
      fun error () = raiseSys (getErrno ())
      fun checkReturnResult (n: int) = if n = ~1 then error () else n
      fun checkReturnPosition (n: Position.int) =
	 if n = ~1 then error () else n
      fun checkResult n = (ignore (checkReturnResult n); ())

      structure SysCall =
	 struct
	    structure Thread = Primitive.Thread

	    val blocker: (unit -> (unit -> unit)) ref =
	       ref (fn () => raise Fail "blocker not installed")
	    val restartFlag = ref true
	       
	    val syscall: {restart: bool} * 
	                 (unit -> int * (unit -> 'a)) -> 'a =
	       fn ({restart}, f) =>
	       let
		  fun call (err: int -> 'a): 'a =
		     let
			val () = Thread.atomicBegin ()
			val (n, post) = f ()
		     in
			if n = ~1
			   then let val e = getErrno ()
				in Thread.atomicEnd () ; err e
				end
			   else (post () before Thread.atomicEnd ())
		     end
		  fun err (e: int): 'a =
		     if restart andalso e = intr andalso !restartFlag
			then if Thread.canHandle () = 0
				then call err
				else let val finish = !blocker ()
				     in
					DynamicWind.wind
					(fn () => call raiseSys, finish)
				     end
			else raiseSys e
	       in
		  call err
	       end

	    local
	       val simpleResult' = fn ({restart}, f) =>
		  syscall ({restart = restart}, fn () => let val n = f () in (n, fn () => n) end)
	    in
	       val simpleResultRestart = fn f =>
		  simpleResult' ({restart = true}, f)
	       val simpleResult = fn f =>
		  simpleResult' ({restart = false}, f)
	    end

	    val simpleRestart = ignore o simpleResultRestart
	    val simple = ignore o simpleResult
	 end
   end
