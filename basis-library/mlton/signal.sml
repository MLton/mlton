(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Signal: MLTON_SIGNAL =
struct

open Posix.Signal
structure Prim = PosixPrimitive.Signal
structure Error = PosixError

local open Prim
in
   val prof = prof
   val vtalrm = vtalrm
end

type how = Prim.how

val toString = SysWord.toString o toWord
   
val checkResult = Error.checkResult

structure Mask =
   struct
      datatype t =
	 All
       | Some of signal list

      val all = All
      val some = Some
	 
      local
	 fun make (how: how) (m: t) =
	    (case m of
		All => checkResult (Prim.sigfillset ())
	      | Some signals =>
		   (checkResult (Prim.sigemptyset ())
		    ; List.app (checkResult o Prim.sigaddset) signals)
		   ; checkResult (Prim.sigprocmask how))
      in
	 val block = make Prim.block
	 val unblock = make Prim.unblock
	 val set = make Prim.setmask
      end
   end

structure Handler =
   struct
      datatype t =
	 Default
       | Ignore
       | Handler of unit Thread.t -> unit Thread.t

      fun simple f = Handler (fn t => (f (); t))
   
      (* I use the "let" so that the dead code elimination can completely
       * eliminate this if getHandler and setHandler are unused.
       *)
      val (get, set) =
	 let
	    val default = checkResult o Prim.default
	    val ignore = checkResult o Prim.ignore
	    val handlee = checkResult o Prim.handlee
	    fun msg s = TextIO.output (TextIO.stdErr, s)
	    val handlers =
	       Array.tabulate (Prim.numSignals, fn s =>
			       if Prim.isDefault s
				  then Default
			       else Ignore)
	    val () =
	       Cleaner.addNew
	       (Cleaner.atSaveWorld, fn () =>
		Array.modify (fn _ => Default) handlers)
	    val () =
	       Cleaner.addNew
	       (Cleaner.atLoadWorld, fn () =>
		Array.appi
		(fn (s, h) =>
		 if Prim.isDefault s
		    then ()
		 else Array.update (handlers, s, Ignore))
		(handlers, 0, NONE))
	    fun getHandler s = Array.sub (handlers, s)
	    (* As far as C is concerned, there is only one signal handler.
	     * As soon as possible after a C signal is received, this signal
	     * handler walks over the array of all SML handlers, and invokes any
	     * one for which a C signal has been received.
	     *)
	    val () =
	       Thread.setHandler
	       (fn t =>
		Array.foldli
		(fn (s, h, t) =>
		 case h of
		    Handler f =>
		       (if Prim.isPending s
			   then let val _ = Thread.state := Thread.InHandler t
				    val t = (f t handle _ =>
					     die "Signal handler raised exception.\n")
				in Thread.state := Thread.Normal
				   ; t
				end
			else t)
		  | _ => t)
		t
		(handlers, 0, NONE))
	    fun setHandler (s, h) =
	       let
		  val old = getHandler s
		  val _ = Array.update (handlers, s, h)
	       in
		  case (old, h) of
		     (Default, Default) => ()
		   | (Ignore, Ignore) => ()
		   | (Handler _, Handler _) => ()
		   | (_, Default) => default s
		   | (_, Ignore) => ignore s
		   | (_, Handler _) => handlee s
	       end
	 in
	    (getHandler, setHandler)
	 end
   end

end
