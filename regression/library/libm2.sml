val () = print "libm2 starting up\n"
val () = OS.Process.atExit 
         (fn () => (_import "m1_close" public : unit -> unit; ()
                   ; print "libm2 exits\n"))

(* Prepare libm1 *)
val () = _import "m1_open" public : int * string vector -> unit; 
         (1, Vector.fromList ["libm1"])

type p = MLton.Pointer.t
type 'a s = (unit -> 'a) * ('a -> unit)
val (_, setSI) = _symbol "libm2smlSymPrivate" alloc private : p s;
val (_, setSB) = _symbol "libm2smlSymPublic"  alloc public  : p s;
val (_, setCI) = _symbol "libm2cSymPrivate" private : p s;
val (_, setCB) = _symbol "libm2cSymPublic"  public  : p s;

type i = (unit -> p)
type e = i -> unit
val () = _export "libm2smlFnPrivate" private : e;
         (fn () => _address "libm2smlSymPrivate" private : p;)
val () = _export "libm2smlFnPublic" public : e;
         (fn () => _address "libm2smlSymPublic" public : p;)
val getCI = _import "libm2cFnPrivate" private : i;
val getCB = _import "libm2cFnPublic" public : i;

(* Store our idea of what the function pointers are in symbols *)
val () = setSI (_address "libm2smlFnPrivate" private : p;)
val () = setSB (_address "libm2smlFnPublic"  public  : p;)
val () = setCI (_address "libm2cFnPrivate"   private : p;)
val () = setCB (_address "libm2cFnPublic"    public  : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "libm2confirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("libm2cFnPrivate", getCI () = _address "libm2cSymPrivate" private : p;)
val () = check ("libm2cFnPublic",  getCB () = _address "libm2cSymPublic"  public  : p;)

(* Test symbols in libm1 *)
val (SB, _) = _symbol "libm1smlSymPublic" public : p s;
val (CB, _) = _symbol "libm1cSymPublic"   public : p s;
val getSB = _import "libm1smlFnPublic" public : i;
val getCB = _import "libm1cFnPublic"   public : i;

(* Check function pointers *)
val () = check ("libm1smlFnPublic", SB () = _address "libm1smlFnPublic" public : p;)
val () = check ("libm1cFnPublic",   CB () = _address "libm1cFnPublic"   public : p;)
(* Check symbol pointers *)
val () = check ("libm1smlSymPublic", getSB () = _address "libm1smlSymPublic" public : p;)
val () = check ("libm1cSymPublic",   getCB () = _address "libm1cSymPublic"   public : p;)

val () = print "m2 pointer test complete.\n"
