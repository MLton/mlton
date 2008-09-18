(* Prepare lib5 *)
val () = _import "lib5_open" : unit -> unit; ()

type p = MLton.Pointer.t

type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "checksmlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "checksmlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "checkcSymPrivate" private : p s;
val (_, setCB) = _symbol "checkcSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "checksmlFnPrivate" private : e;
         (fn () => _address "checksmlSymPrivate" private : p;)
val () = _export "checksmlFnPublic" public : e;
         (fn () => _address "checksmlSymPublic" public : p;)
val getCI = _import "checkcFnPrivate" private : i;
val getCB = _import "checkcFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "checksmlFnPrivate" : p;)
val () = setSB (_address "checksmlFnPublic"  : p;)
val () = setCI (_address "checkcFnPrivate"   : p;)
val () = setCB (_address "checkcFnPublic"    : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "checkconfirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("checkcFnPrivate", getCI () = _address "checkcSymPrivate" private : p;)
val () = check ("checkcFnPublic",  getCB () = _address "checkcSymPublic"  public  : p;)

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

(* Test symbols in lib5 *)
val (SB, _) = _symbol "lib5smlSymPublic" public : p s;
val (CB, _) = _symbol "lib5cSymPublic"   public : p s;
val getSB = _import "lib5smlFnPublic" public : i;
val getCB = _import "lib5cFnPublic"   public : i;

(* Check function pointers *)
val () = check ("lib5smlFnPublic", SB () = _address "lib5smlFnPublic" public : p;)
val () = check ("lib5cFnPublic",   CB () = _address "lib5cFnPublic"   public : p;)
(* Check symbol pointers *)
val () = check ("lib5smlSymPublic", getSB () = _address "lib5smlSymPublic" public : p;)
val () = check ("lib5cSymPublic",   getCB () = _address "lib5cSymPublic"   public : p;)

val () = print "check pointer test complete.\n"
