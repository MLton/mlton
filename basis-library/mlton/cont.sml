structure MLtonCont:> MLTON_CONT =
struct

structure Thread' = MLtonThread
structure Thread = Primitive.Thread

(* This mess with dummy is so that if callcc is ever used anywhere in the
 * program, then Primitive.usesCallcc is set to true during basis library
 * evaluation.  This relies on the dead code elimination algorithm
 * (core-ml/dead-code.fun), which will keep dummy around only if callcc is used.
 *)
val dummy =
   (Primitive.usesCallcc := true
    ; fn () => ())

type 'a t = (unit -> 'a) -> unit

fun callcc (f: 'a t -> 'a): 'a =
   (dummy ()
    ; if Thread'.amInSignalHandler ()
	 then die "callcc can not be used in a signal handler\n"
      else 
	 let
	    datatype 'a state =
	       Original of 'a t -> 'a
	     | Copy of unit -> 'a
	     | Clear
	    val r: 'a state ref = ref (Original f)
	    val _ = Thread.atomicBegin () (* Match 1 *)
	    val _ = Thread.copyCurrent ()
	 in
	    case (!r before r := Clear) of
	       Clear => raise Fail "callcc saw Clear"
	     | Copy v => (Thread.atomicEnd () (* Match 2 *)
			  ; v ())
	     | Original f =>
		  let
		     val t = Thread.savedPre ()
		  in
		     Thread.atomicEnd () (* Match 1 *)
		     ; f (fn v =>
			  let
			     val _ = Thread.atomicBegin () (* Match 2 *)
			     val _ = r := Copy v
			     val new = Thread.copy t
			     (* The following Thread.atomicBegin () 
			      * is matched by Thread.switchTo.
			      *)
			     val _ = Thread.atomicBegin ()
			  in
			     Thread.switchTo new
			  end)
		  end
	 end)

fun ('a, 'b) throw' (k: 'a t, v: unit -> 'a): 'b =
   (k v; raise Fail "throw bug")
   
fun ('a, 'b) throw (k: 'a t, v: 'a): 'b = throw' (k, fn () => v)

fun prepend (k, f) v = throw' (k, f o v)

end
