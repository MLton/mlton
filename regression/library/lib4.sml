(* Prepare lib3 *)
val () = _import "lib3_open" public : int * string vector -> unit; 
         (1, Vector.fromList ["lib3"])

type p = MLton.Pointer.t
type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "lib4smlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "lib4smlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "lib4cSymPrivate" private : p s;
val (_, setCB) = _symbol "lib4cSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "lib4smlFnPrivate" private : e;
         (fn () => _address "lib4smlSymPrivate" private : p;)
val () = _export "lib4smlFnPublic" public : e;
         (fn () => _address "lib4smlSymPublic" public : p;)
val getCI = _import "lib4cFnPrivate" private : i;
val getCB = _import "lib4cFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "lib4smlFnPrivate" : p;)
val () = setSB (_address "lib4smlFnPublic"  : p;)
val () = setCI (_address "lib4cFnPrivate"   : p;)
val () = setCB (_address "lib4cFnPublic"    : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "lib4confirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("lib4cFnPrivate", getCI () = _address "lib4cSymPrivate" private : p;)
val () = check ("lib4cFnPublic",  getCB () = _address "lib4cSymPublic"  public  : p;)

(* Test symbols in lib1 *)
val (SB, _) = _symbol "lib1smlSymPublic" external : p s;
val (CB, _) = _symbol "lib1cSymPublic"   external : p s;
val getSB = _import "lib1smlFnPublic" external : i;
val getCB = _import "lib1cFnPublic"   external : i;

(* Check function pointers *)
val () = check ("lib1smlFnPublic", SB () = _address "lib1smlFnPublic" external : p;)
val () = check ("lib1cFnPublic",   CB () = _address "lib1cFnPublic"   external : p;)
(* Check symbol pointers *)
val () = check ("lib1smlSymPublic", getSB () = _address "lib1smlSymPublic" external : p;)
val () = check ("lib1cSymPublic",   getCB () = _address "lib1cSymPublic"   external : p;)

(* Test symbols in lib2 *)
val (SB, _) = _symbol "lib2smlSymPublic" external : p s;
val (CB, _) = _symbol "lib2cSymPublic"   external : p s;
val getSB = _import "lib2smlFnPublic" external : i;
val getCB = _import "lib2cFnPublic"   external : i;

(* Check function pointers *)
val () = check ("lib2smlFnPublic", SB () = _address "lib2smlFnPublic" external : p;)
val () = check ("lib2cFnPublic",   CB () = _address "lib2cFnPublic"   external : p;)
(* Check symbol pointers *)
val () = check ("lib2smlSymPublic", getSB () = _address "lib2smlSymPublic" external : p;)
val () = check ("lib2cSymPublic",   getCB () = _address "lib2cSymPublic"   external : p;)

(* Test symbols in lib3 *)
val (SB, _) = _symbol "lib3smlSymPublic" public : p s;
val (CB, _) = _symbol "lib3cSymPublic"   public : p s;
val getSB = _import "lib3smlFnPublic" public : i;
val getCB = _import "lib3cFnPublic"   public : i;

(* Check function pointers *)
val () = check ("lib3smlFnPublic", SB () = _address "lib3smlFnPublic" public : p;)
val () = check ("lib3cFnPublic",   CB () = _address "lib3cFnPublic"   public : p;)
(* Check symbol pointers *)
val () = check ("lib3smlSymPublic", getSB () = _address "lib3smlSymPublic" public : p;)
val () = check ("lib3cSymPublic",   getCB () = _address "lib3cSymPublic"   public : p;)

val () = print "lib4 pointer test complete.\n"
