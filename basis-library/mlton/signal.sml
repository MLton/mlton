(* Copyright (C) 1997-1999 NEC Research Institute.
 * Please see the file LICENSE for license information.
 *)
structure Signal: MLTON_SIGNAL =
struct

open Posix.Signal
structure Prim = PosixPrimitive.Signal
structure Error = PosixError

type t = signal
   
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

datatype handler =
   Default
 | Ignore
 | Handler of unit Thread.t -> unit Thread.t

val (get, set, handlers) =
   let
      val handlers =
	 Array.tabulate (Prim.numSignals, fn s =>
			 if Prim.isDefault s
			    then Default
			 else Ignore)
      val _ =
	 Cleaner.addNew
	 (Cleaner.atSaveWorld, fn () =>
	  Array.modify (fn _ => Default) handlers)

      val _ =
	 Cleaner.addNew
	 (Cleaner.atLoadWorld, fn () =>
	  Array.appi
	  (fn (s, h) =>
	   if Prim.isDefault s
	      then ()
	   else Array.update (handlers, s, Ignore))
	  (handlers, 0, NONE))
   in
      (fn s => Array.sub (handlers, s),
       fn (s, h) => Array.update (handlers, s, h),
       handlers)
   end

val getHandler = get

fun ignore s =
   case get s of
      Ignore => ()
    | _ => (set (s, Ignore)
	    ; checkResult (Prim.ignore s))

fun handleDefault s =
   case getHandler s of
      Default => ()
    | _ => (set (s, Default)
	    ; checkResult (Prim.default s))
	 
val handleWith' =
   (* This let is used so that Thread.setHandler is only used if
    * Signal.setHandler' is used.  This prevents threads from being part
    * of every program.
    *)
   let
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
   in
      fn (s, f) =>
      let
	 val old = getHandler s
	 val _ = set (s, Handler f)
      in
	 case old of
	    Handler _ => ()
	  | _ => checkResult (Prim.handlee s)
      end
   end

fun handleWith (s, f) = handleWith' (s, fn t => (f (); t))

fun setHandler (s, h) =
   case h of
      Default => handleDefault s
    | Handler f => handleWith' (s, f)
    | Ignore => ignore s

end
