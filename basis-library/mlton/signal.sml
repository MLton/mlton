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

(* val toString = SysWord.toString o toWord *)
   
val checkResult = Error.checkResult

structure Mask =
   struct
      datatype t =
	 AllBut of signal list
       | Some of signal list

      val allBut = AllBut
      val some = Some

      val all = allBut []
      val none = some []

      local
	 fun member (sigs, s) = List.exists (fn s' => s = s') sigs
	 fun inter (sigs1, sigs2) =
	    List.filter (fn s => member (sigs2, s)) sigs1
	 fun diff (sigs1, sigs2) =
	    List.filter (fn s => not (member (sigs2, s))) sigs1
	 fun union (sigs1, sigs2) =
	    List.foldl (fn (s,sigs) => if member (sigs, s) then sigs else s::sigs) sigs1 sigs2
      in
	 fun block (mask1, mask2) =
	    case (mask1, mask2) of
	       (AllBut sigs1, AllBut sigs2) => AllBut (inter (sigs1, sigs2))
	     | (AllBut sigs1, Some sigs2) => AllBut (diff (sigs1, sigs2))
	     | (Some sigs1, AllBut sigs2) => AllBut (diff (sigs2, sigs1))
	     | (Some sigs1, Some sigs2) => Some (union (sigs1, sigs2)) 
	 fun unblock (mask1, mask2) =
	    case (mask1, mask2) of
	       (AllBut sigs1, AllBut sigs2) => Some (diff (sigs2, sigs1))
	     | (AllBut sigs1, Some sigs2) => AllBut (union (sigs1, sigs2))
	     | (Some sigs1, AllBut sigs2) => Some (inter (sigs1, sigs2))
	     | (Some sigs1, Some sigs2) => Some (diff (sigs1, sigs2))
	 val member = fn (mask, s) =>
	    case mask of
	       AllBut sigs => not (member (sigs, s))
	     | Some sigs => member (sigs, s)
      end

      fun create m =
	 case m of
	    AllBut signals =>
	       (checkResult (Prim.sigfillset ())
		; List.app (checkResult o Prim.sigdelset) signals)
	  | Some signals =>
	       (checkResult (Prim.sigemptyset ())
		; List.app (checkResult o Prim.sigaddset) signals)
	       
      local
	 val blocked = ref none
	    
	 fun make (m: t) =
	    (create m
	     ; checkResult (Prim.sigprocmask ())
	     ; blocked := m)
      in
	 val block = fn m => make (block (!blocked, m))
	 val unblock = fn m => make (unblock (!blocked, m))
	 val setBlocked = fn m => make m
	 val getBlocked = fn () => !blocked
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

fun raiseInval () =
   let
      open PosixError
   in
      raiseSys inval
   end

val (getHandler, set, handlers) =
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
		   val t =
		      Array.foldli
		      (fn (s, h, t) =>
		       case h of
			  Handler f =>
			     if Prim.isPending (fromInt s) then f t else t
			| _ => t)
		      t
		      handlers
		   val t =
		      case !gcHandler of
			 Handler f => if Prim.isGCPending () then f t else t
		       | _ => t
		in
		   t
		end)
	 in
	    Handler
	 end

      fun simple f = handler (fn t => (f (); t))
   end

fun setHandler (s, h) =
   case (getHandler s, h) of
      (InvalidSignal, _) => raiseInval ()
    | (_, InvalidSignal) => raiseInval ()
    | (Default, Default) => ()
    | (_, Default) => 
	 (set (s, Default)
	  ; checkResult (Prim.default s))
    | (Handler _, Handler _) =>
	 set (s, h)
    | (_, Handler _) =>
	 (set (s, h)
	  ; checkResult (Prim.handlee s))
    | (Ignore, Ignore) => ()
    | (_, Ignore) => 
	 (set (s, Ignore)
	  ; checkResult (Prim.ignore s))

fun suspend m =
   (Mask.create m
    ; Prim.suspend ()
    ; MLtonThread.switchToHandler ())

fun handleGC f =
   (Prim.handleGC ()
    ; gcHandler := Handler.simple f)

end
