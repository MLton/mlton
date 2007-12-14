(* main.sml *)

(* Declare ffi to be implemented by calling the C function ffi. *)
val ffi = _import "ffi": real array * int ref * char ref * int -> char;
open Array

val size = 10
val a = tabulate (size, fn i => real i)
val ri = ref 0
val rc = ref #"0"
val n = 17

(* Call the C function *)
val c = ffi (a, ri, rc, n)

val (nGet, nSet) = _symbol "FFI_INT": (unit -> int) * (int -> unit);

val _ = print (concat [Int.toString (nGet ()), "\n"])

val _ =
   print (if c = #"c" andalso !ri = 45 andalso !rc = c
             then "success\n"
          else "fail\n")
