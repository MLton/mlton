val () = print "libm3 starting up\n"
val () = OS.Process.atExit 
         (fn () => (_import "m2_close" public : unit -> unit; ()
                   ; print "libm3 exits\n"))

(* Prepare libm2 *)
val () = _import "m2_open" external : int * string vector -> unit; 
         (1, Vector.fromList ["libm2"])

type p = MLton.Pointer.t
type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "libm3smlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "libm3smlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "libm3cSymPrivate" private : p s;
val (_, setCB) = _symbol "libm3cSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "libm3smlFnPrivate" private : e;
         (fn () => _address "libm3smlSymPrivate" private : p;)
val () = _export "libm3smlFnPublic" public : e;
         (fn () => _address "libm3smlSymPublic" public : p;)
val getCI = _import "libm3cFnPrivate" private : i;
val getCB = _import "libm3cFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "libm3smlFnPrivate" private : p;)
val () = setSB (_address "libm3smlFnPublic"  public  : p;)
val () = setCI (_address "libm3cFnPrivate"   private : p;)
val () = setCB (_address "libm3cFnPublic"    public  : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "libm3confirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("libm3cFnPrivate", getCI () = _address "libm3cSymPrivate" private : p;)
val () = check ("libm3cFnPublic",  getCB () = _address "libm3cSymPublic"  public  : p;)

(* Test symbols in libm1 *)
val (SB, _) = _symbol "libm1smlSymPublic" external : p s;
val (CB, _) = _symbol "libm1cSymPublic"   external : p s;
val getSB = _import "libm1smlFnPublic" external : i;
val getCB = _import "libm1cFnPublic"   external : i;

(* Check function pointers *)
val () = check ("libm1smlFnPublic", SB () = _address "libm1smlFnPublic" external : p;)
val () = check ("libm1cFnPublic",   CB () = _address "libm1cFnPublic"   external : p;)
(* Check symbol pointers *)
val () = check ("libm1smlSymPublic", getSB () = _address "libm1smlSymPublic" external : p;)
val () = check ("libm1cSymPublic",   getCB () = _address "libm1cSymPublic"   external : p;)

(* Test symbols in libm2 *)
val (SB, _) = _symbol "libm2smlSymPublic" external : p s;
val (CB, _) = _symbol "libm2cSymPublic"   external : p s;
val getSB = _import "libm2smlFnPublic" external : i;
val getCB = _import "libm2cFnPublic"   external : i;

(* Check function pointers *)
val () = check ("libm2smlFnPublic", SB () = _address "libm2smlFnPublic" external : p;)
val () = check ("libm2cFnPublic",   CB () = _address "libm2cFnPublic"   external : p;)
(* Check symbol pointers *)
val () = check ("libm2smlSymPublic", getSB () = _address "libm2smlSymPublic" external : p;)
val () = check ("libm2cSymPublic",   getCB () = _address "libm2cSymPublic"   external : p;)

val () = print "m3 pointer test complete.\n"
