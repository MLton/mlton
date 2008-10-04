val () = print "check starting up\n"
val () = OS.Process.atExit 
         (fn () => (_import "m5_close" public : unit -> unit; ()
                   ; print "check exits\n"))

(* Prepare lib5 *)
val () = _import "m5_open" public : int * string vector -> unit; 
         (1, Vector.fromList ["libm5"])

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
val () = setSI (_address "checksmlFnPrivate" private : p;)
val () = setSB (_address "checksmlFnPublic"  public  : p;)
val () = setCI (_address "checkcFnPrivate"   private : p;)
val () = setCB (_address "checkcFnPublic"    public  : p;)

(* Have C confirm that it sees the same function pointers we do.
 * C will check the values of the variables against it's own pointers.
 * C also checks SML functions return his idea of pointers to our exports.
 *)
val () = _import "checkconfirmC" private : unit -> unit; ()

(* Confirm that C functions return pointers to address as we expect. *)
fun check (s, b) = if b then () else print (s ^ " pointers don't match!\n")
val () = check ("checkcFnPrivate", getCI () = _address "checkcSymPrivate" private : p;)
val () = check ("checkcFnPublic",  getCB () = _address "checkcSymPublic"  public  : p;)

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

(* Test symbols in libm5 *)
val (SB, _) = _symbol "libm5smlSymPublic" public : p s;
val (CB, _) = _symbol "libm5cSymPublic"   public : p s;
val getSB = _import "libm5smlFnPublic" public : i;
val getCB = _import "libm5cFnPublic"   public : i;

(* Check function pointers *)
val () = check ("libm5smlFnPublic", SB () = _address "libm5smlFnPublic" public : p;)
val () = check ("libm5cFnPublic",   CB () = _address "libm5cFnPublic"   public : p;)
(* Check symbol pointers *)
val () = check ("libm5smlSymPublic", getSB () = _address "libm5smlSymPublic" public : p;)
val () = check ("libm5cSymPublic",   getCB () = _address "libm5cSymPublic"   public : p;)

val () = print "check pointer test complete.\n"
