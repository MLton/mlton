(* Copyright (C) 2004 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under the GNU General Public License (GPL).
 * Please see the file MLton-LICENSE for license information.
 *)
structure MLtonThread:> MLTON_THREAD_EXTRA =
struct

structure Prim = Primitive.Thread

structure AtomicState =
   struct
      datatype t = NonAtomic | Atomic of int
   end

local
   open Prim
in
   val atomicBegin = atomicBegin
   val atomicEnd = atomicEnd
   val atomicState = fn () =>
      case canHandle () of
	 0 => AtomicState.NonAtomic
       | n => AtomicState.Atomic n
end

fun atomically f =
   (atomicBegin (); DynamicWind.wind (f, atomicEnd))

datatype 'a thread =
   Dead
 | New of 'a -> unit
 (* In Paused (f, t), f is guaranteed to not raise an exception. *)
 | Paused of ((unit -> 'a) -> unit) * Prim.thread

datatype 'a t = T of 'a thread ref

fun prepend (T r: 'a t, f: 'b -> 'a): 'b t =
   let
      val t =
	 case !r of
	    Dead => raise Fail "prepend to a Dead thread"
	  | New g => New (g o f)
	  | Paused (g, t) => Paused (fn h => g (f o h), t)
   in r := Dead
      ; T (ref t)
   end

fun new f = T (ref (New f))

local
   local
      val func: (unit -> unit) option ref = ref NONE
      val base: Prim.preThread =
	 let
	    val () = Prim.copyCurrent ()
	 in
	    case !func of
	       NONE => Prim.savedPre ()
	     | SOME x =>
		  (* This branch never returns. *)
		  let
		     (* Atomic 1 *)
		     val () = func := NONE
		     val () = atomicEnd ()
		     (* Atomic 0 *)
		  in
		     (x () handle e => MLtonExn.topLevelHandler e)
		     ; die "Thread didn't exit properly.\n"
		  end
	 end
   in
      fun newThread (f: unit -> unit) : Prim.thread =
	 let
	    (* Atomic 2 *)
	    val () = func := SOME f
	 in
	    Prim.copy base
	 end
   end
   val switching = ref false
in
   fun ('a, 'b) atomicSwitch' (f: 'a t -> 'b t * (unit -> 'b)): 'a =
      (* Atomic 1 *)
      if !switching
	 then let
		 val () = atomicEnd ()
		 (* Atomic 0 *)
	      in
		 raise Fail "nested Thread.switch"
	      end
      else
	 let
	    val _ = switching := true
	    val r : (unit -> 'a) ref = 
	       ref (fn () => die "Thread.atomicSwitch' didn't set r.\n")
	    val t: 'a thread ref =
	       ref (Paused (fn x => r := x, Prim.current ()))
	    fun fail e = (t := Dead
			  ; switching := false
			  ; atomicEnd ()
			  ; raise e)	
	    val (T t': 'b t, x: unit -> 'b) = f (T t) handle e => fail e
	    val primThread =
	       case !t' before t' := Dead of
		  Dead => fail (Fail "switch to a Dead thread")
		| New g => (atomicBegin (); newThread (g o x))
		| Paused (f, t) => (f x; t)
	    val _ = switching := false
	    (* Atomic 1 when Paused, Atomic 2 when New *)
	    val _ = Prim.switchTo primThread (* implicit atomicEnd() *)
	    (* Atomic 0 when resuming *)
	 in
	    !r ()
	 end

   fun switch' f =
      (atomicBegin ()
       ; atomicSwitch' f)

(* 
   (* One-shot continuations. *)
   fun 'a atomicEscape' (T t : 'a t, x : unit -> 'a) : 'b =
      let
	 val switchee : Prim.thread =
	    case !t before t := Dead of
	       Dead => raise (Fail "escape to a Dead thread")
	     | New g => (atomicBegin (); newThread (g o x))
	     | Paused (f, t) => (f x; t)
      in
	 Prim.switchTo switchee
	 ; die "Thread.atomicEscape' reached impossible.\n"
      end
   fun 'a atomicEscape (t : 'a t, v : 'a) : 'b =
      atomicEscape' (t, fn () => v)
   fun escape' (t, x) =
      (atomicBegin ()
       ; atomicEscape' (t, x))
   fun escape (t, x) =
      (atomicBegin ()
       ; atomicEscape (t, x))

   fun 'a atomicCapture (f: 'a t -> 'a) : 'a =
      let
	 val r : (unit -> 'a) ref = 
	    ref (fn () => die "Thread.atomicCapture didn't set r.\n")
	 val t : 'a t = 
	    T (ref (Paused (fn x => r := x, Prim.current ())))
	 val switcher : Prim.thread =
	    (atomicBegin ()
	     ; newThread (fn () => 
			  let val v = f t
			  in escape (t, v)
			  end 
			  handle e =>
			     escape' (t, fn () => raise e)))
	 val _ = Prim.switchTo switcher
      in
	 !r ()
      end
   fun capture f =
      (atomicBegin ()
       ; atomicCapture f)

   fun ('a, 'b) atomicSwitch' (f: 'a t -> 'b t * (unit -> 'b)): 'a =
      if !switching
	 then (atomicEnd ()
	       ; raise Fail "nested Thread.switch")
      else
	 let
	    val () = switching := true
	    fun finish v () = (switching := false; atomicEnd (); v ())
	    fun fail e = finish (fn () => raise e) ()
	    val v = capture (fn t => 
			     let val (t', v') = f t
			     in escape' (t', finish v')
			     end)
	            handle e => fail e
	 in
	    v
	 end
*)
end

fun atomicSwitch f =
   atomicSwitch' (fn t => let val (t, x) = f t
			  in (t, fn () => x)
			  end)
fun switch f =
   (atomicBegin ()
    ; atomicSwitch f)


fun fromPrimitive (t: Prim.thread): unit t =
   let
      fun f x =
	 x ()
	 handle _ => 
	    die "Asynchronous exceptions are not allowed.\n"
   in
      T(ref(Paused (f,t)))
   end

fun toPrimitive (t as T r : unit t): Prim.thread =
   case !r of
      Dead => die "toPrimitive of Dead.\n"
    | Paused (f, t) =>
	 (r := Dead
	  ; f (fn () => ()) 
	  ; t)
    | New _ =>
	 switch' 
	 (fn cur: Prim.thread t =>
	  (t: unit t, fn () => 
	   switch 
	   (fn t : unit t => 
	    (cur, toPrimitive t))))


local
   val signalHandler: Prim.thread option ref = ref NONE
   datatype state = Normal | InHandler
   val state: state ref = ref Normal
in
   fun amInSignalHandler () = InHandler = !state

   fun setHandler (f: unit t -> unit t): unit =
      let
	 val _ = Primitive.installSignalHandler ()
	 fun loop (): unit =
	    let
	       (* Atomic 1 *)
	       val _ = state := InHandler
	       val t = f (fromPrimitive (Prim.saved ()))
	       val _ = state := Normal
	       val _ = Prim.finishHandler ()
	       val _ =
		  atomicSwitch'
		  (fn (T r) =>
		   let
		      val _ =
			 case !r of
			    Paused (f, _) => f (fn () => ())
			  | _ => raise Fail "setHandler saw strange thread"
		   in
		      (t, fn () => ())
		   end) (* implicit atomicEnd () *)
	    in
	       loop ()
	    end
	 val p =
	    toPrimitive
	    (new (fn () => loop () handle e => MLtonExn.topLevelHandler e))
	 val _ = signalHandler := SOME p
      in
	 Prim.setHandler p
      end

   fun switchToHandler () =
      let
	 (* Atomic 0 *)
	 val () = atomicBegin ()
         (* Atomic 1 *)
	 val () = Prim.startHandler ()
         (* Atomic 2 *)
      in
	 case !signalHandler of
	    NONE => raise Fail "no signal handler installed"
	  | SOME t => Prim.switchTo t (* implicit atomicEnd() *)
      end
end


local

in
   val register: int * (unit -> unit) -> unit =
      let
	 val exports = Array.array (Primitive.FFI.numExports, fn () =>
				    raise Fail "undefined export")
	 fun loop (): unit =
	    let
	       (* Atomic 2 *)
	       val t = Prim.saved ()
	       fun doit () =
		  let
		     (* Atomic 1 *)
		     val _ = 
			(* atomicEnd() after getting args *)
			(Array.sub (exports, Primitive.FFI.getOp ()) ())
			handle e => 
			   (TextIO.output 
			    (TextIO.stdErr, "Call from C to SML raised exception.\n")
			    ; MLtonExn.topLevelHandler e)
			(* atomicBegin() before putting res *)
		     (* Atomic 1 *)
		     val _ = Prim.setSaved t
		     val _ = Prim.returnToC () (* implicit atomicEnd() *)
		  in
		     ()
		  end
	       val _ = Prim.switchTo (toPrimitive (new doit)) (* implicit atomicEnd() *)
	    in
	       loop ()
	    end
	 val p = toPrimitive (new (fn () => loop ()))
	 val _ = Prim.setCallFromCHandler p
      in
	 fn (i, f) => Array.update (exports, i, f)
      end
end

end
