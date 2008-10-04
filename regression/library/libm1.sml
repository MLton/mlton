val () = print "libm1 starting up\n"
val () = OS.Process.atExit (fn () => print "libm1 exits\n")

type p = MLton.Pointer.t

type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "libm1smlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "libm1smlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "libm1cSymPrivate" private : p s;
val (_, setCB) = _symbol "libm1cSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "libm1smlFnPrivate" private : e;
         (fn () => _address "libm1smlSymPrivate" private : p;)
val () = _export "libm1smlFnPublic" public : e;
         (fn () => _address "libm1smlSymPublic" public : p;)
val getCI = _import "libm1cFnPrivate" private : i;
val getCB = _import "libm1cFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "libm1smlFnPrivate" private : p;)
val () = setSB (_address "libm1smlFnPublic"  public  : p;)
val () = setCI (_address "libm1cFnPrivate"   private : p;)
val () = setCB (_address "libm1cFnPublic"    public  : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "libm1confirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("libm1cFnPrivate", getCI () = _address "libm1cSymPrivate" private : p;)
val () = check ("libm1cFnPublic",  getCB () = _address "libm1cSymPublic"  public  : p;)

val () = print "m1 pointer test complete.\n"
