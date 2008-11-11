(* main.sml *)

(* Declare ffi to be implemented by calling the C function ffi. *)
val ffi_addr = _address "ffi" public: MLton.Pointer.t;
val ffi_schema = _import * : MLton.Pointer.t -> real array * int * int ref * char ref * int -> char;
open Array

val size = 10
val a = tabulate (size, fn i => real i)
val ri = ref 0
val rc = ref #"0"
val n = 17

(* Call the C function *)
val c = ffi_schema ffi_addr (a, Array.length a, ri, rc, n)

val _ =
   print (if c = #"c" andalso !ri = 45 andalso !rc = c
             then "success\n"
          else "fail\n")

val n = #1 (_symbol "FFI_INT" public: (unit -> int) * (int -> unit);) ()
val _ = print (concat [Int.toString n, "\n"])
val w = #1 (_symbol "FFI_WORD" public: (unit -> word) * (word -> unit);) ()
val _ = print (concat [Word.toString w, "\n"])
val b = #1 (_symbol "FFI_BOOL" public: (unit -> bool) * (bool -> unit);) ()
val _ = print (concat [Bool.toString b, "\n"])
val r = #1 (_symbol "FFI_REAL" public: (unit -> real) * (real -> unit);) ()
val _ = print (concat [Real.toString r, "\n"])

signature OPAQUE =
   sig
      type t
      val toString : t -> string
   end

structure OpaqueInt :> OPAQUE =
   struct
      type t = Int.int
      val toString = Int.toString
   end
structure OpaqueWord :> OPAQUE =
   struct
      type t = Word.word
      val toString = Word.toString
   end
structure OpaqueBool :> OPAQUE =
   struct
      type t = Bool.bool
      val toString = Bool.toString
   end
structure OpaqueReal :> OPAQUE =
   struct
      type t = Real.real
      val toString = Real.toString
   end

val (n, _) = _symbol "FFI_INT" public: (unit -> OpaqueInt.t) * (OpaqueInt.t -> unit);
val _ = print (concat [OpaqueInt.toString (n ()), "\n"])
val (w, _) = _symbol "FFI_WORD" public: (unit -> OpaqueWord.t) * (OpaqueWord.t -> unit);
val _ = print (concat [OpaqueWord.toString (w ()), "\n"])
val (b, _) = _symbol "FFI_BOOL" public: (unit -> OpaqueBool.t) * (OpaqueBool.t -> unit);
val _ = print (concat [OpaqueBool.toString (b ()), "\n"])
val (r, _) = _symbol "FFI_REAL" public: (unit -> OpaqueReal.t) * (OpaqueReal.t -> unit);
val _ = print (concat [OpaqueReal.toString (r ()), "\n"])

val n_addr = _address "FFI_INT" public: MLton.Pointer.t;
val n = MLton.Pointer.getInt32 (n_addr, 0);
val _ = print (concat [Int.toString n, "\n"])
val w_addr = _address "FFI_WORD" public: MLton.Pointer.t;
val w = MLton.Pointer.getWord32 (w_addr, 0);
val _ = print (concat [Word.toString w, "\n"])
val b_addr = _address "FFI_BOOL" public: MLton.Pointer.t;
val b = (MLton.Pointer.getInt32 (n_addr, 0)) <> 0
val _ = print (concat [Bool.toString b, "\n"])
val r_addr = _address "FFI_REAL" public: MLton.Pointer.t;
val r = MLton.Pointer.getReal64 (r_addr, 0)
val _ = print (concat [Real.toString r, "\n"])
