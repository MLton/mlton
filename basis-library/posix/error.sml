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

      val cleared : syserror = 0

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
      structure Old =
	 struct
	    fun error () = raiseSys (getErrno ())
	    fun checkReturnResult (n: int) = if n = ~1 then error () else n
	    fun checkResult n = (ignore (checkReturnResult n); ())
	    val getErrno = getErrno
	 end

      structure SysCall =
	 struct
	    structure Thread = Primitive.Thread

	    val blocker: (unit -> (unit -> unit)) ref =
	       ref (fn () => (fn () => ()))
	       (* ref (fn () => raise Fail "blocker not installed") *)
	    val restartFlag = ref true

	    val syscallErr: {clear: bool, restart: bool} * 
	                    (unit -> {return: int,
				      post: unit -> 'a,
				      handlers: (syserror * (unit -> 'a)) list}) -> 'a =
	       fn ({clear, restart}, f) =>
	       let
		  fun call (err: {errno: syserror,
				  handlers: (syserror * (unit -> 'a)) list} -> 'a): 'a =
		     let
			val () = Thread.atomicBegin ()
			val () = if clear then clearErrno () else ()
			val {return, post, handlers} = 
			   f () handle exn => (Thread.atomicEnd (); raise exn)
		     in
			if return = ~1
			   then let val e = getErrno ()
				in Thread.atomicEnd () ; err {errno = e, handlers = handlers}
				end
			   else DynamicWind.wind (post, Thread.atomicEnd)
		     end
		  fun err {default: unit -> 'a, 
			   errno: syserror, 
			   handlers: (syserror * (unit -> 'a)) list}: 'a =
		     case List.find (fn (e',_) => errno = e') handlers of
			SOME (_, handler) => handler ()
		      | NONE => default ()
		  fun errBlocked {errno: syserror, handlers: (syserror * (unit -> 'a)) list}: 'a =
		     err {default = fn () => raiseSys errno, 
			  errno = errno, handlers = handlers}
		  fun errUnblocked {errno: syserror, handlers: (syserror * (unit -> 'a)) list}: 'a =
		     err {default = fn () =>
			  if restart andalso errno = intr andalso !restartFlag
			     then if Thread.canHandle () = 0
				     then call errUnblocked
				     else let val finish = !blocker ()
					  in 
					     DynamicWind.wind
					     (fn () => call errBlocked, finish)
					  end
			     else raiseSys errno,
			  errno = errno, handlers = handlers}
	       in
		  call errUnblocked
	       end

	    local
	       val simpleResult' = fn ({restart}, f) =>
		  syscallErr 
		  ({clear = false, restart = restart}, fn () => 
		   let val return = f () 
		   in {return = return, post = fn () => return, handlers = []}
		   end)
	    in
	       val simpleResultRestart = fn f =>
		  simpleResult' ({restart = true}, f)
	       val simpleResult = fn f =>
		  simpleResult' ({restart = false}, f)
	    end
	 
            val simpleRestart = ignore o simpleResultRestart
	    val simple = ignore o simpleResult

	    val syscallRestart = fn f => 
	       syscallErr 
	       ({clear = false, restart = true}, fn () => 
		let val (return, post) = f () 
		in {return = return, post = post, handlers = []}
		end)
	    val syscall = fn f =>
	       syscallErr 
	       ({clear = false, restart = false}, fn () => 
		let val (return, post) = f () 
		in {return = return, post = post, handlers = []}
		end)
	 end
   end
