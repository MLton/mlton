(* main.sml *)

val bool0 = _prim "BOOL0": bool;
val bool1 = _prim "BOOL1": bool;
val int0 = _prim "INT0": int;
val int1 = _prim "INT1": int;
val int2 = _prim "INT2": int;
val real0 = _prim "REAL0": real;
val real1 = _prim "REAL1": real;
val string0 = _prim "STRING0": string;
val word0 = _prim "WORD0": word;
val word1 = _prim "WORD1": word;

val _ =
   if bool0 = false
      andalso bool1 = true
      andalso int0 = ~1
      andalso int1 = 0
      andalso int2 = 1
      andalso Real.== (real0, ~1.234)
      andalso Real.== (real1, 1.234)
      andalso string0 = "hello there\nhow are you\n"
      andalso word0 = 0wx0
      andalso word1 = 0wxFFFFFFFF
      then ()
   else raise Fail "bug"

(* Declare ffi to be implemented by calling the C function ffi. *)
val ffi = _ffi "ffi": real array * int ref * int -> char;
open Array

val size = _prim "FFI_SIZE": int;
val a = tabulate (size, fn i => real i)
val r = ref 0
val n = 17

(* Call the C function *)
val c = ffi (a, r, n)

val n = _ffi "FFI_INT": int;

val _ = print (concat [Int.toString n, "\n"])

val _ =
   print (if c = #"c" andalso !r = 45
	     then "success\n"
	  else "fail\n")
