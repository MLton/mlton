(* Prepare lib1 *)
val () = _import "lib1_open" public : int * string vector -> unit; 
         (1, Vector.fromList ["lib1"])

type p = MLton.Pointer.t
type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "lib2smlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "lib2smlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "lib2cSymPrivate" private : p s;
val (_, setCB) = _symbol "lib2cSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "lib2smlFnPrivate" private : e;
         (fn () => _address "lib2smlSymPrivate" private : p;)
val () = _export "lib2smlFnPublic" public : e;
         (fn () => _address "lib2smlSymPublic" public : p;)
val getCI = _import "lib2cFnPrivate" private : i;
val getCB = _import "lib2cFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "lib2smlFnPrivate" private : p;)
val () = setSB (_address "lib2smlFnPublic"  public  : p;)
val () = setCI (_address "lib2cFnPrivate"   private : p;)
val () = setCB (_address "lib2cFnPublic"    public  : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "lib2confirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("lib2cFnPrivate", getCI () = _address "lib2cSymPrivate" private : p;)
val () = check ("lib2cFnPublic",  getCB () = _address "lib2cSymPublic"  public  : p;)

(* Test symbols in lib1 *)
val (SB, _) = _symbol "lib1smlSymPublic" public : p s;
val (CB, _) = _symbol "lib1cSymPublic"   public : p s;
val getSB = _import "lib1smlFnPublic" public : i;
val getCB = _import "lib1cFnPublic"   public : i;

(* Check function pointers *)
val () = check ("lib1smlFnPublic", SB () = _address "lib1smlFnPublic" public : p;)
val () = check ("lib1cFnPublic",   CB () = _address "lib1cFnPublic"   public : p;)
(* Check symbol pointers *)
val () = check ("lib1smlSymPublic", getSB () = _address "lib1smlSymPublic" public : p;)
val () = check ("lib1cSymPublic",   getCB () = _address "lib1cSymPublic"   public : p;)

val () = print "lib2 pointer test complete.\n"
