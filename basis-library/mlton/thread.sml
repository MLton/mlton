structure Thread:> MLTON_THREAD_EXTRA =
struct

structure Prim = Primitive.Thread

local
   open Prim
in
   val atomicBegin = atomicBegin
   val atomicEnd = atomicEnd
end

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
	    Dead => raise Fail "Thread.prepend"
	  | New g => New (g o f)
	  | Paused (g, t) => Paused (fn h => g (f o h), t)
   in r := Dead
      ; T (ref t)
   end

datatype state =
   Normal
 | InHandler
    
val state = ref Normal

fun new f = T (ref (New f))
   
local
   val func: (unit -> unit) option ref = ref NONE
   val base: Prim.preThread = (Prim.copyCurrent (); Prim.savedPre ())
   val _ = (case !func of
	       NONE => ()
	     | SOME x =>
		  (func := NONE
		   (* Close the atomicBegin of the thread that switched to me. *)
		   ; atomicEnd ()
		   ; (x () handle e => Exn.topLevelHandler e)
		   ; die "Thread didn't exit properly.\n"))
   val switching = ref false
in
   fun ('a, 'b) switch'NoAtomicBegin (f: 'a t -> 'b t * (unit -> 'b)): 'a =
      if !switching
	 then (atomicEnd ()
	       ; raise Fail "nested Thread.switch")
      else
	 let
	    val _ = switching := true
	    val r: (unit -> 'a) option ref = ref NONE
	    val t: 'a thread ref =
	       ref (Paused (fn x => r := SOME x, Prim.current ()))
	    fun fail e = (t := Dead
			  ; switching := false
			  ; atomicEnd ()
			  ; raise e)
	    val (T t': 'b t, x: unit -> 'b) = f (T t) handle e => fail e
	    val primThread =
	       case !t' before (t' := Dead; switching := false) of
		  Dead => fail (Fail "switch to a Dead thread")
		| New g => (func := SOME (g o x)
			    ; Prim.copy base
			    ; Prim.saved ())
		| Paused (f, t) => (f x; t)
	    val _ = switching := false
	    val _ = Prim.switchTo primThread
	    (* Close the atomicBegin of the thread that switched to me. *)
	    val _ = atomicEnd ()
	 in
	    case !r of
	       NONE => die "Thread.switch didn't set r.\n"
	     | SOME v => (r := NONE; v ())
	 end
   fun switch' f =
      (atomicBegin ()
       ; switch'NoAtomicBegin f)
end

fun switch f =
   switch' (fn t => let val (t, x) = f t
		    in (t, fn () => x)
		    end)

fun toPrimitive (t as T r : unit t): Prim.thread =
   case !r of
      Dead => die "toPrimitive of Dead.\n"
    | Paused (f, t) =>
	 (r := Dead
	  ; f (fn () => ()) 
	  ; t)
    | New _ =>
	 switch' (fn cur: Prim.thread t =>
		  (t, fn () => switch (fn t => (cur, toPrimitive t))))

fun fromPrimitive (t: Prim.thread): unit t =
   T (ref (Paused
	   (fn f => ((atomicEnd (); f ())
		     handle _ =>
			die "Asynchronous exceptions are not allowed.\n"),
	    t)))

fun setHandler (f: unit t -> unit t): unit =
   let
      val _ = Primitive.installSignalHandler ()
      fun loop () =
	 let
	    (* s->canHandle == 1 *)
	    val t = f (fromPrimitive (Prim.saved ()))
	    val _ = Prim.finishHandler ()
	    val _ =
	       switch'NoAtomicBegin
	       (fn (T r) =>
		let
		   val _ =
		      case !r of
			 Paused (f, _) => f (fn () => ())
		       | _ => raise Fail "setHandler saw strange pause"
		in
		   (t, fn () => ())
		end)
	 in
	    loop ()
	 end
   in
      Prim.setHandler
      (toPrimitive (new (fn () => loop () handle e => Exn.topLevelHandler e)))
   end

type 'a thread = 'a t

end

