(* main.sml *)

(* Declare ffi to be implemented by calling the C function ffi. *)
val ffi_addr = _import # "ffi" : MLton.Pointer.t;
val ffi_schema = _import * : MLton.Pointer.t -> real array * int ref * int -> char;
open Array

(* val size = _const "FFI_SIZE": int; *)
val size = 10
val a = tabulate (size, fn i => real i)
val r = ref 0
val n = 17

(* Call the C function *)
val c = ffi_schema ffi_addr (a, r, n)

val n_addr = _import # "FFI_INT": MLton.Pointer.t;

val _ = print (concat [Int32.toString (MLton.Pointer.getInt32 (n_addr, 0)), "\n"])

val _ =
   print (if c = #"c" andalso !r = 45
	     then "success\n"
	  else "fail\n")
