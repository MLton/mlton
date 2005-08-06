(* main.sml *)

(* Declare ffi to be implemented by calling the C function ffi. *)
val ffi = _import "ffi": real array * int ref * int -> char;
open Array

val size = 10
val a = tabulate (size, fn i => real i)
val r = ref 0
val n = 17

(* Call the C function *)
val c = ffi (a, r, n)

val (addrN, getN, setN) = _symbol "FFI_INT": MLton.Pointer.t, int;

val _ = print (concat [Int.toString (getN ()), "\n"])

val _ =
   print (if c = #"c" andalso !r = 45
	     then "success\n"
	  else "fail\n")
