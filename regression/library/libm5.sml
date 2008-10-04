val () = print "libm5 starting up\n"
val () = OS.Process.atExit 
         (fn () => (_import "m4_close" public : unit -> unit; ()
                   ; print "libm5 exits\n"))

(* Prepare libm4 *)
val () = _import "m4_open" external : int * string vector -> unit; 
         (1, Vector.fromList ["libm4"])

type p = MLton.Pointer.t
type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "libm5smlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "libm5smlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "libm5cSymPrivate" private : p s;
val (_, setCB) = _symbol "libm5cSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "libm5smlFnPrivate" private : e;
         (fn () => _address "libm5smlSymPrivate" private : p;)
val () = _export "libm5smlFnPublic" public : e;
         (fn () => _address "libm5smlSymPublic" public : p;)
val getCI = _import "libm5cFnPrivate" private : i;
val getCB = _import "libm5cFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "libm5smlFnPrivate" private : p;)
val () = setSB (_address "libm5smlFnPublic"  public  : p;)
val () = setCI (_address "libm5cFnPrivate"   private : p;)
val () = setCB (_address "libm5cFnPublic"    public  : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "libm5confirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("libm5cFnPrivate", getCI () = _address "libm5cSymPrivate" private : p;)
val () = check ("libm5cFnPublic",  getCB () = _address "libm5cSymPublic"  public  : p;)

(* Test symbols in libm3 *)
val (SB, _) = _symbol "libm3smlSymPublic" external : p s;
val (CB, _) = _symbol "libm3cSymPublic"   external : p s;
val getSB = _import "libm3smlFnPublic" external : i;
val getCB = _import "libm3cFnPublic"   external : i;

(* Check function pointers *)
val () = check ("libm3smlFnPublic", SB () = _address "libm3smlFnPublic" external : p;)
val () = check ("libm3cFnPublic",   CB () = _address "libm3cFnPublic"   external : p;)
(* Check symbol pointers *)
val () = check ("libm3smlSymPublic", getSB () = _address "libm3smlSymPublic" external : p;)
val () = check ("libm3cSymPublic",   getCB () = _address "libm3cSymPublic"   external : p;)

(* Test symbols in libm4 *)
val (SB, _) = _symbol "libm4smlSymPublic" external : p s;
val (CB, _) = _symbol "libm4cSymPublic"   external : p s;
val getSB = _import "libm4smlFnPublic" external : i;
val getCB = _import "libm4cFnPublic"   external : i;

(* Check function pointers *)
val () = check ("libm4smlFnPublic", SB () = _address "libm4smlFnPublic" external : p;)
val () = check ("libm4cFnPublic",   CB () = _address "libm4cFnPublic"   external : p;)
(* Check symbol pointers *)
val () = check ("libm4smlSymPublic", getSB () = _address "libm4smlSymPublic" external : p;)
val () = check ("libm4cSymPublic",   getCB () = _address "libm4cSymPublic"   external : p;)

val () = print "m5 pointer test complete.\n"
