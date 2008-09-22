type p = MLton.Pointer.t

type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "lib1smlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "lib1smlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "lib1cSymPrivate" private : p s;
val (_, setCB) = _symbol "lib1cSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "lib1smlFnPrivate" private : e;
         (fn () => _address "lib1smlSymPrivate" private : p;)
val () = _export "lib1smlFnPublic" public : e;
         (fn () => _address "lib1smlSymPublic" public : p;)
val getCI = _import "lib1cFnPrivate" private : i;
val getCB = _import "lib1cFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "lib1smlFnPrivate" private : p;)
val () = setSB (_address "lib1smlFnPublic"  public  : p;)
val () = setCI (_address "lib1cFnPrivate"   private : p;)
val () = setCB (_address "lib1cFnPublic"    public  : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "lib1confirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("lib1cFnPrivate", getCI () = _address "lib1cSymPrivate" private : p;)
val () = check ("lib1cFnPublic",  getCB () = _address "lib1cSymPublic"  public  : p;)

val () = print "lib1 pointer test complete.\n"
