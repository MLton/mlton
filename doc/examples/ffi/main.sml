(* main.sml *)

(*
 * For now, all the uses of _const are commented out until we figure out if/how
 * support for these will be added back to MLton.
 *)

(* val bool0 = _const "BOOL0": bool;
 * val bool1 = _const "BOOL1": bool;
 * val int0 = _const "INT0": int;
 * val int1 = _const "INT1": int;
 * val int2 = _const "INT2": int;
 * val real0 = _const "REAL0": real;
 * val real1 = _const "REAL1": real;
 * val string0 = _const "STRING0": string;
 * val word0 = _const "WORD0": word;
 * val word1 = _const "WORD1": word;
 * 
 * val _ =
 *    if bool0 = false
 *       andalso bool1 = true
 *       andalso int0 = ~1
 *       andalso int1 = 0
 *       andalso int2 = 1
 *       andalso Real.== (real0, ~1.234)
 *       andalso Real.== (real1, 1.234)
 *       andalso string0 = "hello there\nhow are you\n"
 *       andalso word0 = 0wx0
 *       andalso word1 = 0wxFFFFFFFF
 *       then ()
 *    else raise Fail "bug"
 *)

(* Declare ffi to be implemented by calling the C function ffi. *)
val ffi = _ffi "ffi": real array * int ref * int -> char;
open Array

(* val size = _const "FFI_SIZE": int; *)
val size = 10
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
