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
 | InHandler of unit t
    
val state = ref Normal

fun new f = T (ref (New f))
   
local
   val func: (unit -> unit) option ref = ref NONE
   val base: Prim.preThread = (Prim.copyCurrent (); Prim.savedPre ())
   val _ = (case !func of
	       NONE => ()
	     | SOME x =>
		  (func := NONE
		   ; Prim.atomicEnd ()
		   ; (x ()
		      handle e =>
		      die (concat ["Thread raised exception: ",
				   case e of
				      Fail s => concat ["Fail ", s]
				    | _ => exnName e,
			           "\n"]))
		   ; die "Thread didn't exit properly.\n"))
   val switching = ref false
in
   fun ('a, 'b) switch' (f: 'a t -> 'b t * (unit -> 'b)): 'a =
      (Prim.atomicBegin ()  (* matched by Prim.switchTo *)
       ; if !switching
	    then (Prim.atomicEnd ()
		  ; raise Fail "nested Thread.switch")
	 else
	    let
	       val _ = switching := true
	       val r: (unit -> 'a) option ref = ref NONE
	       val t: 'a thread ref =
		  ref (Paused (fn x => r := SOME x, Prim.current ()))
	       fun fail e = (t := Dead
			     ; switching := false
			     ; Prim.atomicEnd ()
			     ; raise e)
	       val (T t': 'b t, x: unit -> 'b) = f (T t) handle e => fail e
	       val primThread =
		  case !t' before t' := Dead of
		     Dead => fail (Fail "switch to a Dead thread")
		   | New g => (Prim.atomicBegin () (* nested *)
			       ; func := SOME (g o x)
			       ; Prim.copy base
			       ; Prim.saved ())
		   | Paused (f, t) => (f x; t)
	       val _ = switching := false
	       val _ = Prim.switchTo primThread
	    in
	       case !r of
		  NONE => die "Throw didn't set r.\n"
		| SOME v => (r := NONE; v ())
	    end)
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
	   (fn f => (f ()
		     handle _ =>
			die "Asynchronous exceptions are not allowed.\n"),
	    t)))

fun setHandler (f: unit t -> unit t): unit =
   let
      fun loop () =
	 (Prim.finishHandler (toPrimitive (f (fromPrimitive (Prim.saved ()))))
	  ; loop ())
   in
      Prim.setHandler (toPrimitive (new loop))
   end

type 'a thread = 'a t

end

