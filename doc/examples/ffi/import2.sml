(* main.sml *)

(* Declare ffi to be implemented by calling the C function ffi. *)
val ffi_addr = _address "ffi" : MLton.Pointer.t;
val ffi_schema = _import * : MLton.Pointer.t -> real array * int ref * int -> char;
open Array

val size = 10
val a = tabulate (size, fn i => real i)
val r = ref 0
val n = 17

(* Call the C function *)
val c = ffi_schema ffi_addr (a, r, n)

val _ =
   print (if c = #"c" andalso !r = 45
             then "success\n"
          else "fail\n")

val n = #1 (_symbol "FFI_INT": (unit -> int) * (int -> unit);) ()
val _ = print (concat [Int.toString n, "\n"])
val w = #1 (_symbol "FFI_WORD": (unit -> word) * (word -> unit);) ()
val _ = print (concat [Word.toString w, "\n"])
val b = #1 (_symbol "FFI_BOOL": (unit -> bool) * (bool -> unit);) ()
val _ = print (concat [Bool.toString b, "\n"])
val r = #1 (_symbol "FFI_REAL": (unit -> real) * (real -> unit);) ()
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

val (n, _) = _symbol "FFI_INT": (unit -> OpaqueInt.t) * (OpaqueInt.t -> unit);
val _ = print (concat [OpaqueInt.toString (n ()), "\n"])
val (w, _) = _symbol "FFI_WORD": (unit -> OpaqueWord.t) * (OpaqueWord.t -> unit);
val _ = print (concat [OpaqueWord.toString (w ()), "\n"])
val (b, _) = _symbol "FFI_BOOL": (unit -> OpaqueBool.t) * (OpaqueBool.t -> unit);
val _ = print (concat [OpaqueBool.toString (b ()), "\n"])
val (r, _) = _symbol "FFI_REAL": (unit -> OpaqueReal.t) * (OpaqueReal.t -> unit);
val _ = print (concat [OpaqueReal.toString (r ()), "\n"])

val n_addr = _address "FFI_INT": MLton.Pointer.t;
val n = MLton.Pointer.getInt32 (n_addr, 0);
val _ = print (concat [Int.toString n, "\n"])
val w_addr = _address "FFI_WORD": MLton.Pointer.t;
val w = MLton.Pointer.getWord32 (w_addr, 0);
val _ = print (concat [Word.toString w, "\n"])
val b_addr = _address "FFI_BOOL": MLton.Pointer.t;
val b = (MLton.Pointer.getInt32 (n_addr, 0)) <> 0
val _ = print (concat [Bool.toString b, "\n"])
val r_addr = _address "FFI_REAL": MLton.Pointer.t;
val r = MLton.Pointer.getReal64 (r_addr, 0)
val _ = print (concat [Real.toString r, "\n"])
