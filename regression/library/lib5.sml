(* Prepare lib4 *)
val () = _import "lib4_open" external : int * string vector -> unit; 
         (1, Vector.fromList ["lib4"])

type p = MLton.Pointer.t
type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "lib5smlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "lib5smlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "lib5cSymPrivate" private : p s;
val (_, setCB) = _symbol "lib5cSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "lib5smlFnPrivate" private : e;
         (fn () => _address "lib5smlSymPrivate" private : p;)
val () = _export "lib5smlFnPublic" public : e;
         (fn () => _address "lib5smlSymPublic" public : p;)
val getCI = _import "lib5cFnPrivate" private : i;
val getCB = _import "lib5cFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "lib5smlFnPrivate" private : p;)
val () = setSB (_address "lib5smlFnPublic"  public  : p;)
val () = setCI (_address "lib5cFnPrivate"   private : p;)
val () = setCB (_address "lib5cFnPublic"    public  : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "lib5confirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("lib5cFnPrivate", getCI () = _address "lib5cSymPrivate" private : p;)
val () = check ("lib5cFnPublic",  getCB () = _address "lib5cSymPublic"  public  : p;)

(* Test symbols in lib3 *)
val (SB, _) = _symbol "lib3smlSymPublic" external : p s;
val (CB, _) = _symbol "lib3cSymPublic"   external : p s;
val getSB = _import "lib3smlFnPublic" external : i;
val getCB = _import "lib3cFnPublic"   external : i;

(* Check function pointers *)
val () = check ("lib3smlFnPublic", SB () = _address "lib3smlFnPublic" external : p;)
val () = check ("lib3cFnPublic",   CB () = _address "lib3cFnPublic"   external : p;)
(* Check symbol pointers *)
val () = check ("lib3smlSymPublic", getSB () = _address "lib3smlSymPublic" external : p;)
val () = check ("lib3cSymPublic",   getCB () = _address "lib3cSymPublic"   external : p;)

(* Test symbols in lib4 *)
val (SB, _) = _symbol "lib4smlSymPublic" external : p s;
val (CB, _) = _symbol "lib4cSymPublic"   external : p s;
val getSB = _import "lib4smlFnPublic" external : i;
val getCB = _import "lib4cFnPublic"   external : i;

(* Check function pointers *)
val () = check ("lib4smlFnPublic", SB () = _address "lib4smlFnPublic" external : p;)
val () = check ("lib4cFnPublic",   CB () = _address "lib4cFnPublic"   external : p;)
(* Check symbol pointers *)
val () = check ("lib4smlSymPublic", getSB () = _address "lib4smlSymPublic" external : p;)
val () = check ("lib4cSymPublic",   getCB () = _address "lib4cSymPublic"   external : p;)

val () = print "lib5 pointer test complete.\n"
