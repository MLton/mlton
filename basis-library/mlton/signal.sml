(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure MLtonSignal: MLTON_SIGNAL_EXTRA =
struct

open Posix.Signal
structure Prim = PosixPrimitive.Signal
structure Error = PosixError

type t = signal

val prof = Prim.prof
val vtalrm = Prim.vtalrm

type how = Prim.how

(* val toString = SysWord.toString o toWord *)
   
val checkResult = Error.checkResult
val checkReturnResult = Error.checkReturnResult
fun raiseInval () =
   let
      open PosixError
   in
      raiseSys inval
   end

val validSignals = 
   Array.tabulate 
   (Prim.numSignals, fn i => 
    Prim.sigismember(fromInt i) <> ~1)

structure Mask =
   struct
      datatype t =
	 AllBut of signal list
       | Some of signal list

      val allBut = AllBut
      val some = Some

      val all = allBut []
      val none = some []

      fun read () =
	 Some
	 (Array.foldri
	  (fn (i, b, sigs) =>
	   if b
	      then if checkReturnResult(Prim.sigismember(fromInt i)) = 1
		      then (fromInt i)::sigs
		      else sigs
	      else sigs)
	  []
	  validSignals)

      fun write m =
	 case m of
	    AllBut signals =>
	       (checkResult (Prim.sigfillset ())
		; List.app (checkResult o Prim.sigdelset) signals)
	  | Some signals =>
	       (checkResult (Prim.sigemptyset ())
		; List.app (checkResult o Prim.sigaddset) signals)
	       
      local
	 fun make (how: how) (m: t) =
	    (write m; checkResult (Prim.sigprocmask how))
      in
	 val block = make Prim.block
	 val unblock = make Prim.unblock
	 val setBlocked = make Prim.setmask
	 fun getBlocked () = (make Prim.block none; read ())
      end

      local
	 fun member (sigs, s) = List.exists (fn s' => s = s') sigs
      in
	 fun isMember (mask, s) =
	    if Array.sub (validSignals, toInt s)
	       then case mask of
		       AllBut sigs => not (member (sigs, s))
		     | Some sigs => member (sigs, s)		  
	       else raiseInval ()
      end
   end

structure Handler =
   struct
      datatype t =
	 Default
       | Handler of unit MLtonThread.t -> unit MLtonThread.t
       | Ignore
       | InvalidSignal
   end

datatype handler = datatype Handler.t

local
   val r = ref false
in
   fun initHandler (s: signal): Handler.t =
      if 0 = Prim.isDefault (s, r)
	 then if !r
		 then Default
	      else Ignore
      else InvalidSignal
end

val (getHandler, setHandler, handlers) =
   let
      val handlers = Array.tabulate (Prim.numSignals, initHandler o fromInt)
      val _ =
	 Cleaner.addNew
	 (Cleaner.atLoadWorld, fn () =>
	  Array.modifyi (initHandler o fromInt o #1) handlers)
   in
      (fn s: t => Array.sub (handlers, toInt s),
       fn (s: t, h) => if Primitive.MLton.Profile.isOn andalso s = prof
			  then raiseInval ()
		       else Array.update (handlers, toInt s, h),
       handlers)
   end

val gcHandler = ref Ignore

structure Mask =
   struct
      open Mask

      fun handled () =
	 Mask.some
	 (Array.foldri
	  (fn (s, h, sigs) =>
	   case h of 
	      Handler _ => (fromInt s)::sigs
	    | _ => sigs) [] handlers)
   end
   
structure Handler =
   struct
      open Handler

      val default = Default
      val ignore = Ignore

      val isDefault = fn Default => true | _ => false
      val isIgnore = fn Ignore => true | _ => false

      val handler =
	 (* This let is used so that Thread.setHandler is only used if
	  * Handler.handler is used.  This prevents threads from being part
	  * of every program.
	  *)
	 let
	    (* As far as C is concerned, there is only one signal handler.
	     * As soon as possible after a C signal is received, this signal
	     * handler walks over the array of all SML handlers, and invokes any
	     * one for which a C signal has been received.
	     *
	     * Any exceptions raised by a signal handler will be caught by
	     * the topLevelHandler, which is installed in thread.sml.
	     *)
	    val () =
	       MLtonThread.setHandler
	       (fn t =>
		let
		   val mask = Mask.getBlocked ()
		   val () = Mask.block (Mask.handled ())
		   val fs = 
		      case !gcHandler of
			 Handler f => if Prim.isGCPending () then [f] else []
		       | _ => []
		   val fs =
		      Array.foldri
		      (fn (s, h, fs) =>
		       case h of
			  Handler f =>
			     if Prim.isPending (fromInt s) then f::fs else fs
			| _ => fs) fs handlers
		   val () = Prim.resetPending ()
		   val () = Mask.setBlocked mask
		in
		   List.foldl (fn (f, t) => f t) t fs
		end)
	 in
	    Handler
	 end

      fun simple f = handler (fn t => (f (); t))
   end

val setHandler = fn (s, h) =>
   case (getHandler s, h) of
      (InvalidSignal, _) => raiseInval ()
    | (_, InvalidSignal) => raiseInval ()
    | (Default, Default) => ()
    | (_, Default) => 
	 (setHandler (s, Default)
	  ; checkResult (Prim.default s))
    | (Handler _, Handler _) =>
	 setHandler (s, h)
    | (_, Handler _) =>
	 (setHandler (s, h)
	  ; checkResult (Prim.handlee s))
    | (Ignore, Ignore) => ()
    | (_, Ignore) => 
	 (setHandler (s, Ignore)
	  ; checkResult (Prim.ignore s))

fun suspend m =
   (Mask.write m
    ; Prim.suspend ()
    ; MLtonThread.switchToHandler ())

fun handleGC f =
   (Prim.handleGC ()
    ; gcHandler := Handler.simple f)

end
