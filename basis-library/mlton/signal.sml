(* Copyright (C) 1999-2002 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-1999 NEC Research Institute.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure MLtonSignal: MLTON_SIGNAL =
struct

open Posix.Signal
structure Prim = PosixPrimitive.Signal
structure Error = PosixError

type t = signal
   
local
   open Prim
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
       | AllBut of signal list
       | Some of signal list

      val allBut = AllBut
      val some = Some

      val all = allBut []
      val none = some []

      fun create m =
	 case m of
	    AllBut signals =>
	       (checkResult (Prim.sigfillset ())
		; List.app (checkResult o Prim.sigdelset) signals)
	  | Some signals =>
	       (checkResult (Prim.sigemptyset ())
		; List.app (checkResult o Prim.sigaddset) signals)

      local
	 fun make (how: how) (m: t) =
	    (create m; checkResult (Prim.sigprocmask how))
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
       | Handler of unit MLtonThread.t -> unit MLtonThread.t
       | Ignore
       | InvalidSignal
   end

datatype handler = datatype Handler.t

local
   val r = ref false
in
   fun initHandler s =
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

val (get, set, handlers) =
   let
      val handlers = Array.tabulate (Prim.numSignals, initHandler)
      val _ =
	 Cleaner.addNew
	 (Cleaner.atLoadWorld, fn () =>
	  Array.modifyi (initHandler o #1) handlers)
   in
      (fn s => Array.sub (handlers, s),
       fn (s, h) => if Primitive.MLton.Profile.isOn andalso s = prof
		       then raiseInval ()
		    else Array.update (handlers, s, h),
       handlers)
   end

val getHandler = get

fun isHandledDefault s =
   case get s of
      Default => true
    | _ => false

fun isIgnored s =
   case get s of
      Ignore => true
    | _ => false

fun ignore s =
   case get s of
      Ignore => ()
    | InvalidSignal => raiseInval ()
    | _ => (set (s, Ignore)
	    ; checkResult (Prim.ignore s))

fun handleDefault s =
   case getHandler s of
      Default => ()
    | InvalidSignal => raiseInval ()
    | _ => (set (s, Default)
	    ; checkResult (Prim.default s))

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
		Array.foldli
		(fn (s, h, t) =>
		 case h of
		    Handler f => if Prim.isPending s then f t else t
		  | _ => t)
		t
		handlers)
	 in
	    Handler
	 end
   end

fun handleWithSafe (s, h) =
   let
      val old = getHandler s
      val _ = set (s, h)
   in
      case old of
	 Handler _ => ()
       | InvalidSignal => raiseInval ()
       | _ => checkResult (Prim.handlee s)
   end

fun handleWith' (s, f) = handleWithSafe (s, Handler.handler f)

fun handleWith (s, f) = handleWith' (s, fn t => (f (); t))

fun setHandler (s, h) =
   case h of
      Default => handleDefault s
    | Handler f => handleWithSafe (s, Handler f)
    | Ignore => ignore s
    | InvalidSignal => raiseInval ()

fun suspend m =
   (Mask.create m
    ; Prim.suspend ()
    ; MLtonThread.switchToHandler ())
   
end
